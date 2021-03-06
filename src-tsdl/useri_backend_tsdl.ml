(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React
open Tsdl

let err_not_tsdl_file = "not a useri.tsdl file"
let log_err msg = Useri_base.App.backend_log `Error msg

let ( >>= ) x f = match x with
| Error _ as e -> e
| Ok v -> f v

(* Time *)

module Time = struct

  (* Time span *)

  type span = Useri_base.Time.span

  (* Passing time *)

  let pfreqi = Sdl.get_performance_frequency ()
  let pfreq = Int64.to_float pfreqi
  let tick_now () = Sdl.get_performance_counter ()
  let tick_add t0 t1 = Int64.add t0 t1
  let tick_of_secs d = Int64.of_float (d *. pfreq)
  let tick_diff_secs t1 t0 = Int64.(to_float (sub t1 t0)) /. pfreq
  let period_of_hz hz =
    if hz = 0 then Int64.max_int else Int64.div pfreqi (Int64.of_int hz)

  let start = tick_now ()
  let elapsed () = tick_diff_secs (tick_now ()) start

  module Line : sig
    type t
    type action = step:React.step -> now:int64 -> int64 -> unit

    val add_deadline : t -> int64 -> action -> unit
    (** [add_deadline l t action] adds an deadline at absolute tick [t]. *)

    val execute : t -> int64 -> int64
    (** [execute l now] executes all deadlines smaller or equal to
        absolute tick [now] and returns the next deadline. *)

    val deadline : t -> int64

    val create : ?size:int -> unit -> t
  end = struct

    (* Deadlines are sorted on the timeline in a imperative heap. *)

    type action = step:React.step -> now:int64 -> int64 -> unit
    type deadline =                              (* deadline on the timeline. *)
      { time : int64;                            (* absolute deadline time. *)
        action : action }                        (* action. *)

    type t =
      { mutable heap : deadline array;
        mutable max : int; }

    let init_size = 256
    let farthest = { time = Int64.max_int;
                     action = (fun ~step ~now _ -> assert false) }
    let create ?(size = init_size) () =
      { heap = Array.make size farthest; max = -1; }

    let grow h =
      let len = h.max + 1 in
      let heap' = Array.make (2 * len) farthest in
      Array.blit h.heap 0 heap' 0 len; h.heap <- heap'

    let shrink_threshold = 26215
    let shrink h =                                   (* assert (h.max < 0). *)
      if Array.length h.heap < shrink_threshold then () else
      h.heap <- Array.make init_size farthest

    let compare heap i i' = Int64.compare heap.(i).time heap.(i').time
    let swap heap i i' =
      let v = heap.(i) in heap.(i) <- heap.(i'); heap.(i') <- v

    let rec up heap i =
      if i = 0 then () else
      let p = (i - 1) / 2 in                                (* parent index. *)
      if compare heap i p < 0 then (swap heap i p; up heap p)

    let rec down heap max i =
      let start = 2 * i in
      let l = start + 1 in                              (* left child index. *)
      let r = start + 2 in                             (* right child index. *)
      if l > max then () (* no child, stop *) else (* find smallest child k. *)
      let k = if r > max then l else (if compare heap l r < 0 then l else r) in
      if compare heap i k > 0 then (swap heap i k; down heap max k)

    let add_deadline h time action =
      let max = h.max + 1 in
      if max = Array.length h.heap then grow h;
      h.heap.(max) <- {time; action} ; h.max <- max; up h.heap max

    let pop h =                                   (* assert not (h.max < 0). *)
      let last = h.heap.(h.max) in
      h.heap.(h.max) <- farthest;
      h.max <- h.max - 1;
      if h.max < 0 then () else (h.heap.(0) <- last; down h.heap h.max 0)

    let execute h now =
      let rec loop step now =
        if h.max < 0 then (shrink h; Int64.max_int) else
        let time = h.heap.(0).time in
        if time > now then time else
        let action = h.heap.(0).action in
        (action ~step ~now time; pop h; loop step now)
      in
      if h.max < 0 then Int64.max_int else
      let step = React.Step.create () in
      let deadline = loop step now in
      React.Step.execute step;
      deadline

    let deadline h =
      if h.max < 0 then Int64.max_int else
      h.heap.(0).time
  end

  let line = Line.create ()

  let tick span =
    let e, send_e = E.create () in
    let action ~step ~now time = send_e ~step (tick_diff_secs time now) in
    let deadline = tick_add (tick_now ()) (tick_of_secs span) in
    Line.add_deadline line deadline action;
    e

  (* Timing animations *)

  module Refresh = struct
    let refresh_hz, set_refresh_hz = S.create 60
    let set_refresh_hz hz = set_refresh_hz hz
    let scheduled_refresh = ref false
    let refresh, send_raw_refresh = E.create ()
    let send_raw_refresh =
      let last_refresh = ref start in
      fun ?step now ->
        send_raw_refresh ?step (tick_diff_secs now !last_refresh);
        last_refresh := now

    let did_refresh = ref true
    let send now = (* in its own update step *)
      if !did_refresh then () else
      (did_refresh := true; send_raw_refresh now)

    let untils = ref []
    let until_empty () = !untils = []
    let until_add u = untils := u :: !untils
    let until_rem u = untils := List.find_all (fun u' -> u != u') !untils

    let anims = ref []
    let anims_empty () = !anims = []
    let anims_update ~step now =
      anims := List.find_all (fun a -> a ~step now) !anims

    let rec refresh_action ~step ~now _ =
      anims_update ~step now;
      did_refresh := false; (* coordinates with Refresh.send *)
      if until_empty () && anims_empty ()
      then (scheduled_refresh := false)
      else
      let deadline = tick_add now (period_of_hz (S.value refresh_hz)) in
      Line.add_deadline line deadline refresh_action;
      scheduled_refresh := true

    let start now = (* delay in case we are in an update step *)
      Line.add_deadline line now refresh_action;
      scheduled_refresh := true

    let anim_add a now =
      anims := a :: !anims;
      if not !scheduled_refresh then (start now)

    let generate_request _ =
      if !scheduled_refresh then () else
      start (tick_now ())

    let request_refresh () = generate_request ()
    let refresher = ref E.never
    let set_refresher e =
      E.stop (!refresher);
      refresher := E.map generate_request e

    let steady_refresh ~until =
      let uref = ref E.never in
      let u = E.map (fun _ -> until_rem !uref) until in
      uref := u;
      if not !scheduled_refresh
      then (until_add u; start (tick_now ()))
      else (until_add u)
  end

  let count ~until =
    let now = tick_now () in
    let s, set_s = S.create 0. in
    let start = ref (Some now) in
    let stop = E.map (fun _ -> start := None) until in
    let stop () = ignore (stop) (* keep a ref. to avoid gc *); !start in
    let anim ~step now = match stop () with
    | None -> false (* remove anim *)
    | Some start -> set_s ~step (tick_diff_secs now start); true
    in
    Refresh.anim_add anim now; s

  let unit ~span =
    let now = tick_now () in
    let s, set_s = S.create 0. in
    let stop = tick_add now (tick_of_secs span) in
    let anim ~step now =
      if now >= stop then (set_s ~step 1.; false (* remove anim *)) else
      (set_s ~step (1. -. ((tick_diff_secs stop now) /. span)); true)
    in
    Refresh.anim_add anim now; s

  (* Counters *)

  type counter = int64
  let counter () = tick_now ()
  let counter_value start =
    let dt = Int64.sub (tick_now ()) start in
    Int64.(to_float dt) /. pfreq

  (* Pretty printing time *)

  let pp_s = Useri_base.Time.pp_s
  let pp_ms = Useri_base.Time.pp_ms
  let pp_mus = Useri_base.Time.pp_mus
end

(* Surface *)

module Surface = struct

  (* Surface mode *)

  type mode = Useri_base.Surface.mode

  let mode_setter, set_mode_setter = E.create ()
  let mode_setter = E.switch E.never mode_setter
  let set_mode_setter me = set_mode_setter me
  let env_mode, send_env_mode = E.create ()
  let mode = S.hold `Windowed (E.select [env_mode; mode_setter])
  let mode_flip e =
    let flip _ m = Useri_base.Surface.mode_flip m in
    S.sample flip e (S.fix `Windowed (fun pred -> mode, pred))

  let pp_mode = Useri_base.Surface.pp_mode

  (* Surface specification *)

  module Gl = Useri_base.Surface.Gl
  type handle = Useri_base.Surface.handle
  type kind = Useri_base.Surface.kind

  type t =
    { hidpi : bool;
      pos : p2 option;
      size : size2;
      kind : kind;
      handle : handle option;
      mode : mode; }

  let create ?(hidpi = true) ?pos ?(size = V2.v 600. 400.)
      ?(kind = (`Gl Gl.default))
      ?handle
      ?(mode = `Windowed) () =
    { hidpi; pos; size; kind; handle; mode }

  module Window = struct
    let win = ref None

    let pos () = match !win with
    | None -> P2.o
    | Some (win, _) ->
        let x, y = Sdl.get_window_position win in
        P2.v (float x) (float y)

    let size () = match !win with
    | None -> Size2.unit
    | Some (win, _) ->
        let w, h = Sdl.get_window_size win in
        Size2.v (float w) (float h)

    let drawable_size () = match !win with
    | None -> Size2.unit
    | Some (win, _) ->
        let w, h = Sdl.gl_get_drawable_size win in
        Size2.v (float w) (float h)

    let setup_kind = function
    | `Other -> Ok ()
    | `Gl c ->
        let bool b = if b then 1 else 0 in
        let ms_buffers, ms_samples = match c.Gl.multisample with
        | None -> 0, 0
        | Some ms_samples -> 1, ms_samples
        in
        let rsize, gsize, bsize, asize = match c.Gl.colors with
        | `RGBA_8888 -> 8, 8, 8, 8
        | `RGB_565 -> 5, 6, 5, 0
        in
        let dsize = match c.Gl.depth with
        | None -> 0
        | Some `D_16 -> 18
        | Some `D_24 -> 24
        in
        let ssize = match c.Gl.stencil with
        | None -> 0
        | Some `S_8 -> 8
        in
        let set a v = Sdl.gl_set_attribute a v in
        let accelerated () = match c.Gl.accelerated with
        | None -> Ok ()
        | Some a -> set Sdl.Gl.accelerated_visual (bool a)
        in
        set Sdl.Gl.share_with_current_context (bool true)
        >>= fun () -> accelerated ()
        >>= fun () -> set Sdl.Gl.multisamplebuffers ms_buffers
        >>= fun () -> set Sdl.Gl.multisamplesamples ms_samples
        >>= fun () -> set Sdl.Gl.doublebuffer (bool c.Gl.doublebuffer)
        >>= fun () -> set Sdl.Gl.stereo (bool c.Gl.stereo)
        >>= fun () -> set Sdl.Gl.framebuffer_srgb_capable (bool c.Gl.srgb)
        >>= fun () -> set Sdl.Gl.red_size rsize
        >>= fun () -> set Sdl.Gl.green_size gsize
        >>= fun () -> set Sdl.Gl.blue_size bsize
        >>= fun () -> set Sdl.Gl.alpha_size asize
        >>= fun () -> set Sdl.Gl.depth_size dsize
        >>= fun () -> set Sdl.Gl.stencil_size ssize
        >>= fun () -> set Sdl.Gl.context_profile_mask
          Sdl.Gl.context_profile_core
        >>= fun () -> set Sdl.Gl.context_major_version (fst c.Gl.version)
        >>= fun () -> set Sdl.Gl.context_minor_version (snd c.Gl.version)

    let create name s =
      let mode = match s.mode with
      | `Windowed -> Sdl.Window.windowed
      | `Fullscreen -> Sdl.Window.fullscreen_desktop
      in
      let x, y = match s.pos with
      | None -> None, None
      | Some pos -> Some (truncate (V2.x pos)), Some (truncate (V2.y pos))
      in
      let w, h = truncate (Size2.w s.size), truncate (Size2.h s.size) in
      let atts = Sdl.Window.(opengl + resizable + hidden + mode) in
      let atts = if s.hidpi then Sdl.Window.(atts + allow_highdpi) else atts in
      setup_kind s.kind
      >>= fun ()  -> Sdl.create_window ?x ?y ~w ~h name atts
      >>= fun win -> Sdl.gl_create_context win
      >>= fun ctx -> Sdl.gl_make_current win ctx
      >>= fun ()  -> Sdl.gl_set_swap_interval 1
      >>= fun ()  -> Ok (win, ctx)

    let destroy win ctx =
      Sdl.gl_delete_context ctx;
      Sdl.destroy_window win;
      Ok ()
  end

  let handle () = failwith "TODO"

  let set_mode =
    let set mode = match !Window.win with
    | None -> ()
    | Some (win, _) ->
        let f = match mode with
        | `Windowed -> Sdl.Window.windowed
        | `Fullscreen -> Sdl.Window.fullscreen_desktop
        in
        match Sdl.set_window_fullscreen win f with
        | Ok () -> () | Error (`Msg msg) ->
            (* Log.err "app mode change: %s" msg *)
            (* TODO *)
            Printf.eprintf "app mode change: %s" msg
    in
    E.map set (S.changes mode)

  (* Properties *)

  let pos, set_pos = S.create P2.o
  let raster_size, set_raster_size = S.create Size2.unit
  let size, set_size = S.create Size2.unit
  let update () = match !Window.win with
  | Some (win, _) -> Sdl.gl_swap_window win
  | _ -> ()

  (* Refreshing *)

  let refresh = Time.Refresh.refresh
  let request_refresh = Time.Refresh.request_refresh
  let set_refresher = Time.Refresh.set_refresher
  let steady_refresh = Time.Refresh.steady_refresh
  let refresh_hz = Time.Refresh.refresh_hz
  let set_refresh_hz = Time.Refresh.set_refresh_hz

  (* Events *)

  let sdl_window e =
    match Sdl.Event.(window_event_enum (get e window_event_id)) with
    | `Exposed | `Resized ->
        let step = Step.create () in
        set_raster_size ~step (Window.drawable_size ());
        set_pos ~step (Window.pos ());
        set_size ~step (Window.size ());
        Step.execute step;
        (* Avoid simultaneity so that the client can reshape *)
        Time.Refresh.send_raw_refresh (Time.tick_now ())
    | `Moved ->
        let step = Step.create () in
        set_pos ~step (Window.pos ());
        Step.execute step;
    | _ -> ()

  let init step name s =
    Window.create name s >>= fun i ->
    Window.win := Some i;
    send_env_mode ~step s.mode;
    set_pos ~step (Window.pos ());
    set_size ~step (Window.size ());
    set_raster_size ~step (Window.drawable_size ());
    Ok ()

  let show () = match !Window.win with
  | None -> ()
  | Some (win, _) -> Sdl.show_window win

  let release step = match !Window.win with
  | None -> ()
  | Some (win, ctx) -> ignore (Window.destroy win ctx);
end

(* Mouse *)

module Mouse = struct
  let pos, set_pos = S.create V2.zero
  let dpos, send_dpos = E.create ()
  let left, set_left = S.create false
  let left_down, send_left_down = E.create ()
  let left_up, send_left_up = E.create ()
  let middle, set_middle = S.create false
  let middle_down, send_middle_down = E.create ()
  let middle_up, send_middle_up = E.create ()
  let right, set_right = S.create false
  let right_down, send_right_down = E.create ()
  let right_up, send_right_up = E.create ()

  let event_pos size e =
    let x = Sdl.Event.(get e mouse_button_x) in
    let y = Sdl.Event.(get e mouse_button_y) in
    P2.v (float x /. Size2.w size) (1. -. (float y /. Size2.h size))

  let event_dpos size e =
    let xrel = Sdl.Event.(get e mouse_motion_xrel) in
    let yrel = Sdl.Event.(get e mouse_motion_yrel) in
    P2.v (float xrel /. Size2.w size) (-. (float yrel /. Size2.h size))

  let sdl_button_down size e =
    let pos = event_pos size e in
    let update set send =
      let s = Step.create () in
      let step = Some s in
      set ?step true; send ?step pos;
      Step.execute s
    in
    match Sdl.Event.(get e mouse_button_button) with
    | b when b = Sdl.Button.left -> update set_left send_left_down
    | b when b = Sdl.Button.middle -> update set_middle send_middle_down
    | b when b = Sdl.Button.right -> update set_right send_right_down
    | _ -> ()

  let sdl_button_up size e =
    let update set send =
      let pos = event_pos size e in
      let s = Step.create () in
      let step = Some s in
      set ?step false; send ?step pos; Step.execute s;
    in
    match Sdl.Event.(get e mouse_button_button) with
    | b when b = Sdl.Button.left -> update set_left send_left_up
    | b when b = Sdl.Button.middle -> update set_middle send_middle_up
    | b when b = Sdl.Button.right -> update set_right send_right_up
    | _ -> ()

  let sdl_motion size e =
    let pos = event_pos size e in
    let dpos = event_dpos size e in
    let step = Step.create () in
    set_pos ~step pos;
    send_dpos ~step dpos;
    Step.execute step

  let init step = ()
  let release step = ()
end

(* Touch *)

module Touch = struct

  type over = [ `Up | `Cancel ]
  type t =
    { id : int;
      did : int;
      pos : p2 signal * (?step:React.step -> p2 -> unit);
      dpos : v2 event * (?step:React.step -> v2 -> unit);
      pressure : float signal * (?step:React.step -> float -> unit);
      over : over event * (?step:React.step -> over -> unit); }

  let id t = t.id
  let did _ = 0 (* not supported *)
  let pos t = fst t.pos
  let dpos t = fst t.dpos
  let pressure _ = S.const 0. (* not supported *)
  let over t = fst t.over

  (* TODO *)

  let start = E.never
end

(* Keyboard *)

module Key = struct
  type id = Useri_base.Key.id
  let uchar = Useri_base.Key.uchar
  let pp_id = Useri_base.Key.pp_id

  let any_down = Useri_base.Key.any_down
  let any_up = Useri_base.Key.any_up
  let any_holds = Useri_base.Key.any_holds
  let down = Useri_base.Key.down
  let up = Useri_base.Key.up
  let holds = Useri_base.Key.holds

  let alt = Useri_base.Key.alt
  let ctrl = Useri_base.Key.ctrl
  let meta = Useri_base.Key.meta
  let shift = Useri_base.Key.shift

  module Int = struct
    type t = int
    let compare : int -> int -> int = compare
  end

  let id_of_keycode =
    let module Imap = Map.Make (Int) in
    let map = [
      Sdl.K.lalt, `Alt `Left; Sdl.K.ralt, `Alt `Right;
      Sdl.K.up, `Arrow `Up; Sdl.K.down, `Arrow `Down;
      Sdl.K.left, `Arrow `Left; Sdl.K.right, `Arrow `Right;
      Sdl.K.backspace, `Backspace;
      Sdl.K.lctrl, `Ctrl `Left; Sdl.K.rctrl, `Ctrl `Right;
      Sdl.K.k0, `Digit 0; Sdl.K.k1, `Digit 1; Sdl.K.k2, `Digit 2;
      Sdl.K.k3, `Digit 3; Sdl.K.k4, `Digit 4; Sdl.K.k5, `Digit 5;
      Sdl.K.k6, `Digit 6; Sdl.K.k7, `Digit 7; Sdl.K.k8, `Digit 8;
      Sdl.K.k9, `Digit 9;
      Sdl.K.kp_0, `Digit 0; Sdl.K.kp_1, `Digit 1; Sdl.K.kp_2, `Digit 2;
      Sdl.K.kp_3, `Digit 3; Sdl.K.kp_4, `Digit 4; Sdl.K.kp_5, `Digit 5;
      Sdl.K.kp_6, `Digit 6; Sdl.K.kp_7, `Digit 7; Sdl.K.kp_8, `Digit 8;
      Sdl.K.kp_9, `Digit 9;
      Sdl.K.kend, `End;
      Sdl.K.kp_enter, `Enter;
      Sdl.K.escape, `Escape;
      Sdl.K.f1, `Function 1; Sdl.K.f2, `Function 2; Sdl.K.f3, `Function 3;
      Sdl.K.f4, `Function 4; Sdl.K.f5, `Function 5; Sdl.K.f6, `Function 6;
      Sdl.K.f7, `Function 7; Sdl.K.f8, `Function 8; Sdl.K.f9, `Function 9;
      Sdl.K.f10, `Function 10; Sdl.K.f11, `Function 11;
      Sdl.K.f12, `Function 12; Sdl.K.f13, `Function 13;
      Sdl.K.f14, `Function 14; Sdl.K.f15, `Function 15;
      Sdl.K.f16, `Function 16; Sdl.K.f17, `Function 17;
      Sdl.K.f18, `Function 18; Sdl.K.f19, `Function 19;
      Sdl.K.f20, `Function 20; Sdl.K.f21, `Function 21;
      Sdl.K.f22, `Function 22; Sdl.K.f23, `Function 23;
      Sdl.K.f24, `Function 24;
      Sdl.K.home, `Home;
      Sdl.K.lgui, `Meta `Left; Sdl.K.rgui, `Meta `Right;
      Sdl.K.pagedown, `Page `Down;
      Sdl.K.pageup, `Page `Up;
      Sdl.K.return, `Return;
      Sdl.K.lshift, `Shift `Left; Sdl.K.rshift, `Shift `Right;
      Sdl.K.space, `Space;
      Sdl.K.tab, `Tab; ]
    in
    let add acc (k, v) = Imap.add k v acc in
    let m = List.fold_left add Imap.empty map in
    fun kc -> try Imap.find kc m with
    | Not_found ->
        if kc land Sdl.K.scancode_mask > 0
        then `Unknown kc
        else `Uchar (Uchar.of_int kc)

  let sdl_down e =
    let id = id_of_keycode (Sdl.Event.(get e keyboard_keycode)) in
    if Sdl.Event.(get e keyboard_repeat) > 0 then () else
    let step = Step.create () in
    Useri_base.Key.handle_down ~step id;
    Step.execute step;
    ()

  let sdl_up e =
    let id = id_of_keycode (Sdl.Event.(get e keyboard_keycode)) in
    let step = Step.create () in
    Useri_base.Key.handle_up ~step id;
    Step.execute step;
    ()

  let init = Useri_base.Key.init
  let release = Useri_base.Key.release
end

(* Text input *)

module Text = struct

  (* Text keyboard input *)

  let input_enabled = ref (S.const ())
  let set_input_enabled enabled =
    let enabler enabled =
      if enabled then Sdl.start_text_input () else Sdl.stop_text_input ()
    in
    S.stop !input_enabled;
    input_enabled := S.map enabler enabled;
    ()

  let input, send_input = E.create ()
  let editing, send_editing = E.create ()

  let sdl_input e =
    let text = Sdl.Event.(get e text_input_text) in
    send_input text

  let sdl_editing e =
    let text = Sdl.Event.(get e text_editing_text) in
    let start = Sdl.Event.(get e text_editing_start) in
    let len = Sdl.Event.(get e text_editing_length) in
    send_editing (text, start, len)

  (* Clipboard *)

  let sdl_clipboard, sdl_clipboard_send = E.create ()
  let sdl_clipboard_send step =
    if not (Sdl.has_clipboard_text ()) then sdl_clipboard_send ~step "" else
    match Sdl.get_clipboard_text () with
    | Ok c -> sdl_clipboard_send ~step c
    | Error (`Msg e) -> log_err e

  let sdl_clipboard_update () =
    let step = React.Step.create () in
    sdl_clipboard_send step;
    React.Step.execute step

  let app_setters, app_setter_send = E.create ()
  let set_clipboard_setter setter = app_setter_send setter
  let app_clipboard =
    let set v =
      begin match Sdl.set_clipboard_text v with
      | Ok () -> () | Error (`Msg e) -> log_err e
      end;
      v
    in
    E.map set (E.switch E.never app_setters)

  let clipboard = S.hold "" (E.select [sdl_clipboard; app_clipboard])

  let init step = sdl_clipboard_send step
  let release step =
    set_input_enabled (S.const false);
    app_setter_send ~step E.never;
    ()
end

(* File drag and drop *)

module Drop = struct

  type file = Useri_base.Drop.file

  module File = struct
    type t = file
    let inj, proj = Useri_base.Drop.File.create ()

    let to_string f = match proj f with
    | None -> invalid_arg err_not_tsdl_file
    | Some f -> f

    let path f = to_string f
    let prepare f k = k f (Ok ())
  end

  let file, send_file = E.create ()
  let sdl_file e =
    let f = File.inj (Sdl.Event.drop_file_file e) in
    Sdl.Event.drop_file_free e;
    let step = React.Step.create () in
    send_file ~step f;
    React.Step.execute step;
    ()

  let init step = Sdl.set_event_state Sdl.Event.drop_file Sdl.enable
  let release step = ()
end

module Human = struct

  (* System latency feelings *)

  let noticed = Useri_base.Human.noticed
  let interrupted = Useri_base.Human.interrupted
  let left = Useri_base.Human.left

  let rec feel_action feel set_feel ~step ~now _ =
    let new_feel, delay = match S.value feel with
    | `Interacting -> `Interrupted, left -. interrupted
    | `Interrupted -> `Left, 0.
    | `Left -> assert false
    in
    set_feel ~step new_feel;
    if delay = 0. then () else
    let deadline = Time.tick_add now (Time.tick_of_secs delay) in
    Time.Line.add_deadline Time.line deadline (feel_action feel set_feel)

  let feel () =
    let feel, set_feel = S.create `Interacting in
    let set_feel ~step v = set_feel ~step v in
    let now = Time.tick_now () in
    let deadline = Time.tick_add now (Time.tick_of_secs interrupted) in
    Time.Line.add_deadline Time.line deadline (feel_action feel set_feel);
    feel

  (* Touch target and finger size *)

  let touch_target_size = Useri_base.Human.touch_target_size
  let touch_target_size_min = Useri_base.Human.touch_target_size_min
  let touch_target_pad = Useri_base.Human.touch_target_pad
  let average_finger_width = Useri_base.Human.average_finger_width
end


module App = struct

  let prefs_path ~org ~app = Sdl.get_pref_path ~org ~app
  let env k ~default parse = try parse (Sys.getenv k) with _ -> default

  let quit, send_quit = E.create ()

  (* Event and signal sinks *)

  type sink = Esink : 'a event -> sink | Ssink : 'a signal -> sink
  let sinks = ref []
  let sink_event e = sinks := Esink e :: !sinks
  let sink_signal s = sinks := Ssink s :: !sinks
  let release_sinks () =
    let release = function
    | Esink e -> E.stop e
    | Ssink s -> S.stop s
    in
    List.iter release !sinks; sinks := []

  (* Init, run and exit *)

  let start, send_start = E.create ()
  let stop, send_stop = E.create ()

  let running = ref false
  let ensure_start_sent ?step () =
    if not !running then (running := true; send_start ?step ())

  let send_stop ?step () = send_stop ?step (); running := false

  let release ?(sinks = true) () =
    let step = Step.create () in
    send_stop ~step ();
    Mouse.release step;
    Key.release step;
    Text.release step;
    Drop.release step;
    Surface.release step;
    Step.execute step;
    Useri_base.App.(set_backend_logger default_backend_logger);
    if sinks then release_sinks ();
    Sdl.quit ();
    ()

  let e_some = ref (Some (Sdl.Event.create ()))

  let init ?(name = Useri_base.App.default_name) ?(surface = Surface.create ())
      ()
    =
    Sdl.init Sdl.Init.(video + events) >>= fun () ->
    let step = Step.create () in
    Surface.init step name surface >>= fun () ->
    Mouse.init step;
    Key.init step;
    Text.init step;
    Drop.init step;
    Step.execute step;
    Time.Refresh.send_raw_refresh (Time.tick_now ());
    Surface.show ();
    Ok ()

  let do_event e =
    let e = match e with None -> assert false (* TODO invalid_arg *)
                       | Some e -> e
    in
    let event e = Sdl.Event.(enum (get e typ)) in
    match event e with
    | `Quit -> send_quit ()
    | `Key_down -> Key.sdl_down e
    | `Key_up -> Key.sdl_up e
    | `Window_event -> Surface.sdl_window e
    | `Mouse_button_down -> Mouse.sdl_button_down (Surface.Window.size ()) e
    | `Mouse_button_up -> Mouse.sdl_button_up (Surface.Window.size ()) e
    | `Mouse_motion -> Mouse.sdl_motion (Surface.Window.size ()) e
    | `Text_editing -> Text.sdl_editing e
    | `Text_input -> Text.sdl_input e
    | `Clipboard_update -> Text.sdl_clipboard_update ()
    | `Drop_file -> Drop.sdl_file e
    | _ -> ()

  let run_step () =
    ensure_start_sent ();
    while Sdl.poll_event !e_some do do_event !e_some; done;
    let now = Time.tick_now () in
    let deadline = Time.Line.execute Time.line now in
    Time.Refresh.send now;
    Float.fmin 10e-3 (Time.tick_diff_secs deadline now)

  let rec run ?until () =
    let rec loop stop =
      let t = run_step () in
      Sdl.delay (Int32.of_float (t *. 1000.));
      if S.value stop then () else loop stop
    in
    let stop = match until with
    | Some e -> E.stamp e true
    | None -> E.stamp quit true
    in
    ensure_start_sent ();
    loop (S.hold false stop)

  (* Launch context *)

  type launch_context = Useri_base.App.launch_context
  let pp_launch_context = Useri_base.App.pp_launch_context

  let launch_context =
    let getenv var = try Some (Sys.getenv var) with Not_found -> None in
    match Sdl.get_platform () with
    | "Mac OS X" ->
        begin match getenv "TERM" with
        | None -> `Gui
        | _ ->
            match getenv "_" with
            | Some "/usr/bin/open" -> `Gui
            | _ -> `Terminal
        end
    | "Linux" ->
        begin match getenv "TERM" with
        | None | Some "dumb" -> `Gui
        | _ -> `Terminal
        end
    | _ -> `Terminal

  (* Platform and backend *)
  let platform = Sdl.get_platform ()

  type backend = Useri_base.App.backend
  let backend = `Tsdl
  let pp_backend = Useri_base.App.pp_backend
  let set_backend_logger = Useri_base.App.set_backend_logger

  type backend_scheme = Useri_base.App.backend_scheme
  let backend_scheme = `Sync
  let pp_backend_scheme = Useri_base.App.pp_backend_scheme

  (* CPU count *)

  type cpu_count = Useri_base.App.cpu_count
  let cpu_count = `Known (Sdl.get_cpu_count ())
  let pp_cpu_count = Useri_base.App.pp_cpu_count
end


(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
