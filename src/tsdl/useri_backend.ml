(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React
open Tsdl

let str = Format.asprintf 
let execname =
  let base = Filename.basename Sys.argv.(0) in
  try Filename.chop_extension base with 
  | Invalid_argument _ (* this API is pathetic *) -> base

let ( >>= ) x f = match x with
| `Error _ as e -> e
| `Ok v -> f v

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

  let sdl_init step = ()
end

(* Keyboard *) 

module Key = struct

  include Useri_backend_base.Key 

  module Int = struct 
    type t = int 
    let compare : int -> int -> int = Pervasives.compare 
  end
  
  let sym_of_keycode = 
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
        if kc land Sdl.K.scancode_mask > 0 then `Unknown kc else `Uchar kc
            
  let (any_down : sym event), send_any_down = E.create ()
  let (any_repeat : sym event), send_any_repeat = E.create ()
  let (any_up : sym event), send_any_up = E.create ()
  let down_count = ref 0 
  let any_holds, set_any_holds = S.create false 

  let down_event = Hashtbl.create 47 
  let repeat_event = Hashtbl.create 47 
  let up_event = Hashtbl.create 47 

  let def_event event sym = try fst (Hashtbl.find event sym) with 
  | Not_found -> 
      let def = E.create () in 
      Hashtbl.add event sym def; 
      fst def 

  let send_event ?step event sym = 
    try snd (Hashtbl.find event sym) ?step () 
    with Not_found -> ()

  let state_signal = Hashtbl.create 47
  let set_signal ?step (sym : sym) v = 
    try snd (Hashtbl.find state_signal sym) ?step v
    with Not_found -> ()
  
  let down ?(repeat = false) (sym : sym) = 
    if repeat then def_event repeat_event sym else 
    def_event down_event sym 

  let up (sym : sym) = def_event up_event sym
  let holds sym = try fst (Hashtbl.find state_signal sym) with 
  | Not_found ->
      let def = S.create false in (* TODO get state from sdl *)
      Hashtbl.add state_signal sym def;
      fst def

  let meta =
    let def = S.create false in 
    Hashtbl.add state_signal (`Meta `Left) def; 
    Hashtbl.add state_signal (`Meta `Right) def; 
    fst def

  let ctrl =
    let def = S.create false in 
    Hashtbl.add state_signal (`Ctrl `Left) def; 
    Hashtbl.add state_signal (`Ctrl `Right) def; 
    fst def

  let alt =
    let def = S.create false in 
    Hashtbl.add state_signal (`Alt `Left) def; 
    Hashtbl.add state_signal (`Alt `Right) def; 
    fst def

  let sdl_down e = (* TODO correct semantics *)
    let sym = sym_of_keycode (Sdl.Event.(get e keyboard_keycode)) in
    let repeat = Sdl.Event.(get e keyboard_repeat) <> 0 in
    if repeat then begin 
      let step = Step.create () in
      send_any_repeat ~step sym;
      send_event ~step repeat_event sym; 
      Step.execute step
    end else begin 
      let step = Step.create () in
      send_any_down ~step sym;
      send_any_repeat ~step sym;
      incr down_count; set_any_holds true;
      send_event ~step down_event sym;
      send_event ~step repeat_event sym; 
      set_signal ~step sym true;
      Step.execute step
    end

  let sdl_up e = (* TODO correct semantics *)
    let sym = sym_of_keycode (Sdl.Event.(get e keyboard_keycode)) in
    let step = Step.create () in 
    send_any_up ~step sym;
    decr down_count; 
    if !down_count <= 0 then (down_count := 0; set_any_holds false);
    send_event ~step up_event sym;
    set_signal ~step sym false; 
    Step.execute step

  let sdl_init step = ()
end

(* Text input *) 

module Text = struct

  (* Text keyboard input *) 

  let input_enabled, set_input_enabled = S.create false 
  let input, send_input = E.create () 
  let editing, send_editing = E.create ()

  let set_input_enabled b = 
    if b then Sdl.start_text_input () else Sdl.stop_text_input (); 
    set_input_enabled b

  let sdl_editing e = 
    let text = Sdl.Event.(get e text_editing_text) in 
    let start = Sdl.Event.(get e text_editing_start) in 
    let len = Sdl.Event.(get e text_editing_length) in 
    send_editing (text, start, len)
      
  let sdl_input e =
    let text = Sdl.Event.(get e text_input_text) in
    send_input text

  (* Clipboard *)

  let clipboard, set_clipboard = S.create None

  let sdl_clipboard_update step = 
    if not (Sdl.has_clipboard_text ()) then set_clipboard ~step None else
    match Sdl.get_clipboard_text () with 
    | `Ok c -> set_clipboard ~step (Some c) 
    | `Error e -> assert false (* TODO log *)

  let set_clipboard v = set_clipboard ?step:None v 

  let sdl_init step = sdl_clipboard_update step
end

(* File drag and drop *)

module Drop = struct
  let file, send_file = E.create ()

  let sdl_file e = 
    let f = Sdl.Event.drop_file_file e in 
    Sdl.Event.drop_file_free e; 
    send_file f

  let sdl_init step =
    Sdl.set_event_state Sdl.Event.drop_file Sdl.enable
end

(* Time *) 

module Time = struct 
  type span = float 

  let pfreqi = Sdl.get_performance_frequency ()
  let pfreq = Int64.to_float pfreqi
  let tick_now () = Sdl.get_performance_counter ()
  let tick_add t0 t1 = Int64.add t0 t1
  let tick_of_secs d = Int64.of_float (d *. pfreq)
  let tick_diff_secs t1 t0 = Int64.(to_float (sub t1 t0)) /. pfreq
  let period_of_hz hz = 
    if hz = 0 then Int64.max_int else Int64.div pfreqi (Int64.of_int hz)

  let tick_start = tick_now ()
  let elapsed () = tick_diff_secs (tick_now ()) tick_start

  let tick_next now period = 
    let part = Int64.(rem now period) in
    Int64.(add (sub now part) period)
            
  type counter = int64
  let counter () = tick_now () 
  let value start = 
    let dt = Int64.sub (tick_now ()) start in 
    Int64.(to_float dt) /. pfreq
        
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
        mutable action : action }                (* action. *)
    
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

  let pp_s ppf s = Format.fprintf ppf "%gs" s
  let pp_ms ppf s = Format.fprintf ppf "%gms" (s *. 1e3)
  let pp_mus ppf s = Format.fprintf ppf "%gμs" (s *. 1e6)
end

module Human = struct

  let noticed = 0.1 
  let interrupted = 1. 
  let left = 10. 
  let feel () = failwith "TODO"

  (* Sizes in mm. *) 
  let touch_target_size = 9. 
  let touch_target_size_min = 7.
  let touch_target_pad = 2. 
  let average_finger_width = 11.
end

(* Application *)

let app = ref None
let pos, set_pos = S.create P2.o 
let size, set_size = S.create Size2.unit

let window_pos () = match !app with 
| None -> P2.o 
| Some (win, _) -> 
    let x, y = Sdl.get_window_position win in 
    P2.v (float x) (float y)

let window_size () = match !app with 
| None -> Size2.unit
| Some (win, _) -> 
    let w, h = Sdl.get_window_size win in 
    Size2.v (float w) (float h)

let drawable_size () = match !app with 
| None -> Size2.unit 
| Some (win, _) -> 
    let w, h = Sdl.gl_get_drawable_size win in
    Size2.v (float w) (float h)

module Surface = struct

  (* Properties *) 

  let size, set_size = S.create Size2.unit    
  let update () = match !app with 
  | Some (win, _) -> Sdl.gl_swap_window win
  | _ -> ()

  (* Refreshing *) 

  let last_refresh = ref (Time.tick_now ())
  let scheduled_refresh = ref false
  let refresh, send_raw_refresh = E.create ()
  let refresh_hz, set_refresh_hz = S.create 60
  let set_refresh_hz hz = set_refresh_hz hz

  let untils = ref []
  let until_empty () = !untils = []
  let until_add u = untils := u :: !untils 
  let until_rem u = untils := List.find_all (fun u' -> u != u') !untils 

  let anims = ref [] 
  let anims_empty () = !anims = []
  let anim_add a = anims := a :: !anims
  let anims_set_rem ~step now = 
    anims := List.find_all (fun a -> a ~step now) !anims

  let send_simple_refresh ?step () =
    let now = Time.tick_now () in
    send_raw_refresh ?step (Time.tick_diff_secs now !last_refresh);
    last_refresh := now

  let rec refresh_action ~step ~now _ = 
    anims_set_rem ~step now;
    send_raw_refresh ~step (Time.tick_diff_secs now !last_refresh);
    last_refresh := now;
    if until_empty () && anims_empty () 
    then (scheduled_refresh := false)
    else 
    let deadline = Time.tick_add now (Time.period_of_hz (S.value refresh_hz)) in
    Time.Line.add_deadline Time.line deadline refresh_action; 
    scheduled_refresh := true

  let start_refreshes now = (* delay in case we are in an update step *)
    Time.Line.add_deadline Time.line now refresh_action; 
    scheduled_refresh := true
      
  let refresher = ref E.never 

  let generate_request _ = 
    if !scheduled_refresh then () else
    start_refreshes (Time.tick_now ())

  let request_refresh () = generate_request ()

  let set_refresher e =
    E.stop (!refresher); 
    refresher := E.map generate_request e
                                 
  let steady_refresh ~until =
    let uref = ref E.never in
    let u = E.map (fun _ -> until_rem !uref) until in
    uref := u;
    if not !scheduled_refresh
    then (until_add u; start_refreshes (Time.tick_now ()))
    else (until_add u)

  let animate ~span =
    let s, set_s = S.create 0. in
    let now = Time.tick_now () in
    let stop = Time.tick_add now (Time.tick_of_secs span) in
    let a ~step now =
      if now >= stop then (set_s ~step 1.; false (* remove anim *)) else
      (set_s ~step (1. -. ((Time.tick_diff_secs stop now) /. span)); true)
    in
    if not !scheduled_refresh
    then (anim_add a; start_refreshes (Time.tick_now ()); s)
    else (anim_add a; s)
    
  (* Specification *) 

  type colors = [ `RGBA_8888 | `RGB_565 ]
  type depth = [ `D_24 | `D_16 ]
  type stencil = [ `S_8 ]
  type spec = 
    { share : bool; 
      accelerated : bool option; 
      multisample : int option; 
      doublebuffer : bool; 
      stereo : bool; 
      srgb : bool; 
      colors : colors; 
      depth : depth option; 
      stencil : stencil option; 
      gl : int * int; }

  let spec
      ?(share = false)
      ?(accelerated = None)
      ?(multisample = Some 8)
      ?(doublebuffer = true)
      ?(stereo = false)
      ?(srgb = true)
      ?(colors = `RGBA_8888) 
      ?(depth = Some `D_24)
      ?(stencil = None)
      ~gl () =
    { share; accelerated; multisample; doublebuffer; stereo; srgb; colors; 
      depth; stencil; gl }

  let sdl_setup c = 
    let bool b = if b then 1 else 0 in
    let ms_buffers, ms_samples = match c.multisample with 
    | None -> 0, 0
    | Some ms_samples -> 1, ms_samples 
    in
    let rsize, gsize, bsize, asize = match c.colors with 
    | `RGBA_8888 -> 8, 8, 8, 8
    | `RGB_565 -> 5, 6, 5, 0
    in
    let dsize = match c.depth with 
    | None -> 0 
    | Some `D_16 -> 18
    | Some `D_24 -> 24
    in
    let ssize = match c.stencil with 
    | None -> 0 
    | Some `S_8 -> 8 
    in
    let set a v = Sdl.gl_set_attribute a v in
    let accelerated () = match c.accelerated with 
    | None -> `Ok ()
    | Some a -> set Sdl.Gl.accelerated_visual (bool a) 
    in
    set Sdl.Gl.share_with_current_context (bool c.share)        
    >>= fun () -> accelerated ()                                              
    >>= fun () -> set Sdl.Gl.multisamplebuffers ms_buffers                    
    >>= fun () -> set Sdl.Gl.multisamplesamples ms_samples                    
    >>= fun () -> set Sdl.Gl.doublebuffer (bool c.doublebuffer)               
    >>= fun () -> set Sdl.Gl.stereo (bool c.stereo)                           
    >>= fun () -> set Sdl.Gl.framebuffer_srgb_capable (bool c.srgb)           
    >>= fun () -> set Sdl.Gl.red_size rsize                                   
    >>= fun () -> set Sdl.Gl.green_size gsize                                 
    >>= fun () -> set Sdl.Gl.blue_size bsize                                  
    >>= fun () -> set Sdl.Gl.alpha_size asize                                 
    >>= fun () -> set Sdl.Gl.depth_size dsize                                 
    >>= fun () -> set Sdl.Gl.stencil_size ssize                               
    >>= fun () -> set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core 
    >>= fun () -> set Sdl.Gl.context_major_version (fst c.gl)                 
    >>= fun () -> set Sdl.Gl.context_minor_version (snd c.gl)
end

module Window = struct
  let create hidpi pos size name surf_spec mode =
    let mode = match mode with 
    | `Windowed -> Sdl.Window.windowed 
    | `Fullscreen -> Sdl.Window.fullscreen_desktop 
    in
    let x, y = match pos with 
    | None -> None, None
    | Some pos -> Some (truncate (V2.x pos)), Some (truncate (V2.y pos)) 
    in
    let w, h = truncate (Size2.w size), truncate (Size2.h size) in
    let atts = Sdl.Window.(opengl + resizable + hidden + mode) in
    let atts = if hidpi then Sdl.Window.(atts + allow_highdpi) else atts in
    Surface.sdl_setup surf_spec                   
    >>= fun ()  -> Sdl.create_window ?x ?y ~w ~h name atts       
    >>= fun win -> Sdl.gl_create_context win                     
    >>= fun ctx -> Sdl.gl_make_current win ctx                   
    >>= fun ()  -> Sdl.gl_set_swap_interval 1                    
    >>= fun ()  -> `Ok (win, ctx)

  let destroy win ctx =
    Sdl.gl_delete_context ctx;
    Sdl.destroy_window win;
    `Ok ()

  let sdl_window e =
    match Sdl.Event.(window_event_enum (get e window_event_id)) with
    | `Exposed | `Resized -> 
        let step = Step.create () in
        let surface_size = drawable_size () in
        Surface.set_size ~step surface_size;
        set_pos ~step (window_pos ());
        set_size ~step (window_size ());
        Step.execute step; 
        (* Avoid simultaneity so that the client can reshape *)
        Surface.send_simple_refresh ()
    | `Moved -> 
        let step = Step.create () in 
        set_pos ~step (window_pos ());
        Step.execute step;
    | _ -> ()
end

module App = struct
 
  type launch_context = [ `Browser | `Gui | `Terminal ]                        
  let pp_launch_context ppf lc = Format.fprintf ppf begin match lc with 
    | `Browser -> "browser"
    | `Gui -> "gui" 
    | `Terminal -> "terminal" 
    end

  let launch_context =
    let osx_psn a = String.length a > 5 && String.sub a 0 5 = "-psn_" in 
    try 
      for i = 0 to Array.length Sys.argv - 1 do 
        if osx_psn Sys.argv.(i) then raise Exit 
      done;
      `Terminal
    with 
    Exit -> `Gui

  let platform = Sdl.get_platform () 
  let backend_runtime = `Sync
  let cpu_count = Sdl.get_cpu_count ()

  let prefs_path ~org ~app = Sdl.get_pref_path ~org ~app
  let size = size 
  let pos = pos
  let env k ~default parse = try parse (Sys.getenv k) with _ -> default

  let quit, send_quit = E.create ()

  (* Mode *) 

  type mode = [ `Windowed | `Fullscreen ] 

  let mode_switch ?(init = `Windowed) e =
    let switch_mode = function 
    | `Windowed -> `Fullscreen 
    | `Fullscreen -> `Windowed
    in
    S.accum (E.map (fun _ m -> switch_mode m) e) init
           

  let (mode_sig : mode signal signal), set_mode_sig = 
    S.create (S.const `Windowed)    

  let mode = S.switch ~eq:( == ) mode_sig 
  (* TODO maybe it would be better to delay. *)

  let set_mode =
    let set mode = match !app with 
    | None -> ()
    | Some (win, _) -> 
        let f = match mode with
        | `Windowed -> Sdl.Window.windowed 
        | `Fullscreen -> Sdl.Window.fullscreen_desktop 
        in
        match Sdl.set_window_fullscreen win f with 
        | `Ok () -> () | `Error msg -> 
            (* Log.err "app mode change: %s" msg *) 
            (* TODO *) 
            Printf.eprintf "app mode change: %s" msg 
    in
    E.map set (S.changes mode)
      
  (* Event and signal sinks *) 
  
  type sink = Esink : 'a event -> sink | Ssink : 'a signal -> sink 
  let sinks = ref []
  let sink_event e = sinks := Esink e :: !sinks
  let sink_signal s = sinks := Ssink s :: !sinks
  let clear_sinks () = sinks := []; Gc.full_major ()

  (* Init, run and exit *)

  let start, send_start = E.create ()
  let (stop : unit event), send_stop = E.create ()    
  let release () = match !app with
  | None -> ()
  | Some (win, ctx) -> 
      ignore (Window.destroy win ctx); 
      Sdl.quit (); 
      ()

  let e_some = ref None  

  let init ?(hidpi = true) ?pos ?(size = V2.v 600. 400.) 
      ?(name = String.capitalize execname)
      ?(surface = Surface.spec (3,2) ()) ?(mode = S.value mode_sig) () =
    Sdl.init Sdl.Init.(video + events) >>= fun () ->
    Window.create hidpi pos size name surface (S.value mode) >>= fun i ->
    let step = React.Step.create () in
    e_some := Some (Sdl.Event.create ());
    app := Some i;
    set_mode_sig mode;
    set_pos ~step (window_pos ());
    set_size ~step (window_size ());
    Surface.set_size ~step (drawable_size ());
    Mouse.sdl_init step; 
    Key.sdl_init step; 
    Text.sdl_init step;
    Drop.sdl_init step;
    React.Step.execute step;
    let step = React.Step.create () in
    Surface.send_simple_refresh ~step (); 
    send_start ~step ();
    React.Step.execute step;
    (match !app with None -> () | Some (win, _) -> Sdl.show_window win);
    `Ok ()
    
  let do_event e =
    let e = match e with None -> assert false (* TODO invalid_arg *) 
                       | Some e -> e 
    in
    let event e = Sdl.Event.(enum (get e typ)) in
    match event e with 
    | `Quit -> send_quit ()
    | `Key_down -> Key.sdl_down e
    | `Key_up -> Key.sdl_up e
    | `Window_event -> Window.sdl_window e
    | `Mouse_button_down -> Mouse.sdl_button_down (window_size ()) e
    | `Mouse_button_up -> Mouse.sdl_button_up (window_size ()) e
    | `Mouse_motion -> Mouse.sdl_motion (window_size ()) e
    | `Text_editing -> Text.sdl_editing e
    | `Text_input -> Text.sdl_input e
    | `Clipboard_update -> 
        let step = React.Step.create () in
        Text.sdl_clipboard_update step; 
        React.Step.execute step
    | `Drop_file -> Drop.sdl_file e
    | _ -> ()


  let run_step () = 
    while Sdl.poll_event !e_some do do_event !e_some; done;
    let now = Time.tick_now () in 
    let deadline = Time.Line.execute Time.line now in
    Float.fmin 10e-3 (Time.tick_diff_secs deadline now)

  let rec run ~until = 
    let rec loop stop = 
      let t = run_step () in 
      Sdl.delay (Int32.of_float (t *. 1000.));
      if S.value stop then () else loop stop
    in
    loop (S.hold false (E.stamp until true))

end
  

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
