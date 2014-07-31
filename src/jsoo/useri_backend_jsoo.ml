(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React

let str = Format.asprintf
let execname =
  let base = Filename.basename Sys.argv.(0) in
  try Filename.chop_extension base with
  | Invalid_argument _ (* this API is pathetic *) -> base

let log_warn msg = Useri_base.App.backend_log `Warning msg
let log_err msg = Useri_base.App.backend_log `Error msg
let warn_time () = log_warn "performance.now () missing, using Date.now ()"
let warn_drag () = log_warn "Drag.file event not supported"
let warn_but () = log_warn "unexpected e.which"
let err_not_jsoo_anchor = "not a useri.jsoo anchor"
let err_not_jsoo_file = "not a useri.jsoo file"
let err_no_gl = "`Gl unsupported for WebGL use `Other"
let err_init = "Useri not initialized"

(* js_of_ocaml events *)

module Ev = struct
  let ids = ref []
  let release () = List.iter Dom.removeEventListener !ids

  let cb node e f =
    let h = Dom.full_handler (fun n ev -> Js.bool (f n ev)) in
    ids := Dom.addEventListener node e h (Js.bool false) :: !ids;
    ()
end

(* Time *)

module Time = struct

  (* Time span *)

  type span = Useri_base.Time.span

  (* Passing time *)

  let tick_now =
    let date_now () = (jsnew Js.date_now () ## getTime ()) /. 1000. in
    let perf_now () =
      (Js.Unsafe.coerce Dom_html.window) ## performance ## now () /. 1000.
    in
    let perf = (Js.Unsafe.coerce Dom_html.window) ## performance in
    match Js.Optdef.to_option perf with
    | None -> warn_time (); date_now
    | Some p ->
        match (Js.Unsafe.coerce p) ## now with
        | None -> warn_time (); date_now
        | Some n -> perf_now

  let start = tick_now ()
  let elapsed () = tick_now () -. start
  let tick span =
    let e, send_e = E.create () in
    let start = tick_now () in
    let action () = send_e (tick_now () -. start) in
    let ms = span *. 1000. in
    ignore (Dom_html.window ## setTimeout (Js.wrap_callback action, ms));
    e

  (* Counting time *)

  type counter = span
  let counter () = tick_now ()
  let value c = tick_now () -. c

  (* Pretty printing time *)

  let pp_s = Useri_base.Time.pp_s
  let pp_ms = Useri_base.Time.pp_ms
  let pp_mus = Useri_base.Time.pp_mus
end

(* Mouse *)

module Mouse = struct
  let pos, set_pos = S.create P2.o
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

  let event_mouse_pos c e =
    let r = (c :> Dom_html.element Js.t) ## getBoundingClientRect () in
    let x = (float (e ## clientX)) -. r ## left in
    let y = (float (e ## clientY)) -. r ## top in
    let nx = x /. (r ## right -. r ## left) in
    let ny = 1. -. (y /. (r ## bottom -. r ## top)) in
    V2.v nx ny

  let set_mouse_pos ~step c e =
    let epos = event_mouse_pos c e in
    send_dpos ~step V2.(epos - (S.value pos));
    set_pos ~step epos;
    epos

  let down_cb c e =
    let step = Step.create () in
    let epos = set_mouse_pos ~step c e in
    let set, send_down = match Js.Optdef.to_option (e ## which) with
    | Some Dom_html.Left_button -> set_left, send_left_down
    | Some Dom_html.Middle_button -> set_middle, send_middle_down
    | Some Dom_html.Right_button -> set_right, send_right_down
    | None | Some Dom_html.No_button -> warn_but (); set_left, send_left_down
    in
    set ~step true; send_down ~step epos;
    React.Step.execute step;
    ignore ((Js.Unsafe.coerce c) ## focus ()); (* since we prevent default *)
    false

  let up_cb c e =
    let step = Step.create () in
    let epos = set_mouse_pos ~step c e in
    let set, send_up = match Js.Optdef.to_option (e ## which) with
    | Some Dom_html.Left_button -> set_left, send_left_up
    | Some Dom_html.Middle_button -> set_middle, send_middle_up
    | Some Dom_html.Right_button -> set_right, send_right_up
    | None | Some Dom_html.No_button -> warn_but (); set_left, send_left_up
    in
    set ~step false; send_up ~step epos;
    React.Step.execute step;
    false

  let move_cb c e =
    Dom.preventDefault e;
    let step = Step.create () in
    let _ = set_mouse_pos ~step c e in
    React.Step.execute step;
    false
end

(* Key *)

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

  (* For browser keyboard handling see http://unixpapa.com/js/key.html *)

  let id_of_keycode kc = match kc with
  | n when 48 <= n && n <= 57 -> `Digit (n - 48)
  | n when 65 <= n && n <= 90 -> `Uchar n
  | n when 96 <= n && n <= 105 -> `Digit (n - 96)
  | n when 112 <= n && n <= 135 -> `Function (n - 111)
  | 8 -> `Backspace
  | 9 -> `Tab
  | 13 -> `Return
  | 16 -> `Shift `Left
  | 17 -> `Ctrl `Left
  | 18 -> `Alt `Left
  | 27 -> `Escape
  | 32 -> `Space
  | 33 -> `Page `Up
  | 34 -> `Page `Down
  | 35 -> `End
  | 36 -> `Home
  | 37 -> `Arrow `Left
  | 38 -> `Arrow `Up
  | 39 -> `Arrow `Right
  | 40 -> `Arrow `Down
  | 45 -> `Enter
  | 91 | 224 -> `Meta `Left
  | 93 -> `Meta `Right
  | n -> `Unknown n

  let event_target : Dom_html.eventTarget Js.t option ref = ref None
  let set_event_target t = event_target := t
  let event_target () = !event_target

  module Int = struct
    type t = int
    let compare : int -> int -> int = Pervasives.compare
  end

  module Iset = Set.Make (Int)

  let downs = ref Iset.empty (* to suppress key repeat *)

  let down_cb _ e =
    let kc = e ## keyCode in
    if Iset.mem kc !downs then false else
    let id = id_of_keycode kc in
    let step = Step.create () in
    Useri_base.Key.handle_down ~step id;
    Step.execute step;
    downs := Iset.add kc !downs;
    false

  let up_cb _ e =
    let kc = e ## keyCode in
    let id = id_of_keycode kc in
    let step = Step.create () in
    Useri_base.Key.handle_up ~step id;
    Step.execute step;
    downs := Iset.remove kc !downs;
    false

  let setup_cbs c =
    let t = match event_target () with None -> c | Some t -> t in
    Ev.cb t Dom_html.Event.keydown down_cb;
    Ev.cb t Dom_html.Event.keyup up_cb;
    set_event_target (Some t);
    ()

  let init = Useri_base.Key.init
  let release ~step =
    Useri_base.Key.release ~step;
    set_event_target None;
    ()
end

(* Surface *)

let canvas : Dom_html.canvasElement Js.t option ref = ref None

module Surface = struct

  module Gl = Useri_base.Surface.Gl
  type kind = Useri_base.Surface.kind

  let inj, proj = Useri_base.Surface.Anchor.create ()
  let anchor_of_canvas = inj
  let canvas_of_anchor a = match proj a with
  | None -> invalid_arg err_not_jsoo_anchor
  | Some c -> c

  let size : size2 signal = fst (S.create Size2.zero)
  let update : unit -> unit = fun () -> ()

  let anchor () = match !canvas with
  | None -> invalid_arg err_init
  | Some c -> anchor_of_canvas c

  let init ~hidpi ?pos ~size ~surface ?anchor ~mode () =
    let c = match anchor with
    | Some a -> canvas_of_anchor a
    | None ->
        let c = Dom_html.(createCanvas document) in
        Dom.appendChild (Dom_html.document ## body) c;
        c
    in
    match surface with
    | `Other ->
        let topx = str "%dpx" in
        let w = Float.int_of_round (Size2.w size) in
        let h = Float.int_of_round (Size2.h size) in
        c ## style ## width <- Js.string (topx w);
        c ## style ## height <- Js.string (topx h);
        c ## width <- w;
        c ## height <- h;
        Ev.cb c Dom_html.Event.mousedown Mouse.down_cb;
        Ev.cb c Dom_html.Event.mouseup Mouse.up_cb;
        Ev.cb c Dom_html.Event.mousemove Mouse.move_cb;
        c ## setAttribute (Js.string "tabindex", Js.string "1");
(*        (Js.Unsafe.coerce c) ## focus (); *)
        Key.setup_cbs (c :> Dom_html.eventTarget Js.t);
        canvas := Some c
    | `Gl _ -> invalid_arg err_no_gl

  (* Refresh *)

  let scheduled_refresh = ref false
  let refresh, send_raw_refresh = E.create ()
  let send_raw_refresh =
    let last_refresh = ref (Time.tick_now ()) in
    fun ?step now ->
      send_raw_refresh ?step (now -. !last_refresh);
      last_refresh := now

  let refresh_hz, set_refresh_hz = S.create 60
  let set_refresh_hz hz = set_refresh_hz hz

  let untils = ref []
  let untils_empty () = !untils = []
  let until_add u = untils := u :: !untils
  let until_rem u = untils := List.find_all (fun u' -> u != u') !untils

  let anims = ref []
  let anims_empty () = !anims = []
  let anim_add a = anims := a :: !anims
  let anims_update ~step now =
    anims := List.find_all (fun a -> a ~step now) !anims

  let rec refresh_action () =
    let step = Step.create () in
    let now = Time.tick_now () in
    anims_update ~step now;
    send_raw_refresh ~step now;
    Step.execute step;
    if untils_empty () && anims_empty ()
    then (scheduled_refresh := false)
    else start_refreshes ()

  and start_refreshes () =
    let callback = Js.wrap_callback refresh_action in
    Dom_html._requestAnimationFrame callback;
    scheduled_refresh := true

  let generate_request _ =
    if !scheduled_refresh then () else
    start_refreshes ()

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
    then (until_add u; start_refreshes ())
    else (until_add u)

  let animate ~span =
    let s, set_s = S.create 0. in
    let now = Time.tick_now () in
    let stop = now +. span in
    let a ~step now =
      if now >= stop then (set_s ~step 1.; false (* remove anim *)) else
      (set_s ~step (1. -. ((stop -. now) /. span)); true)
    in
    if not !scheduled_refresh
    then (anim_add a; start_refreshes (); s)
    else (anim_add a; s)
end

(* Text *)

module Text = struct

  let (input : string event), send_input = E.create ()
  let (editing : (string * int * int) event), send_editing = E.create ()

  (* The following doesn't handle dead keys correctly. *)

  let hidden_input : Dom_html.inputElement Js.t option ref = ref None
  let input_cb i e =
    if (Js.Optdef.test ((Js.Unsafe.coerce e) ## data))
    then send_input (Js.to_string ((Js.Unsafe.coerce e) ## data))
    else send_input (Js.to_string (i ## value));
    i ## value <- (Js.string "");
    false

  let start_text_input () =
    if true then invalid_arg "Unsupported in this backend" else
    (* This shows how we could solve the problem. With input
       events (supported everywhere) we can't handle deadkeys correctly,
       but textInput does, though we don't get to see the editing events
       like in sdl. I'm not sure it's possible to integrate the scheme
       without getting Key events through the same input field. This
       needs further thinking. *)
    match !canvas with
    | None -> log_err err_init
    | Some canvas ->
        let i = Dom_html.(createInput ~_type:(Js.string "text") document) in
        i ## style ## position <- Js.string "absolute";
        i ## style ## opacity <- Js.def (Js.string "0");
        i ## style ## pointerEvents <- Js.string "none";
        i ## style ## zIndex <- Js.string "0";
        let e =
          if Js.Optdef.test ((Js.Unsafe.variable "window.TextEvent"))
          then (Dom.Event.make "textInput")
          else (Dom_html.Event.input)
        in
        Ev.cb i e input_cb;
        match Js.Opt.to_option ((canvas :> Dom.node Js.t) ## parentNode)
        with
        | None -> log_err "canvas has no parent"
        | Some p ->
            Dom.insertBefore p i (Js.Opt.return canvas);
            ignore ((Js.Unsafe.coerce i) ## focus ());
            hidden_input := Some (i :> Dom_html.inputElement Js.t);
            ()

  let stop_text_input () =
    if true then invalid_arg "Unsupported in this backend" else
    match !hidden_input with
    | None -> ()
    | Some i ->
        hidden_input := None;
        match Js.Opt.to_option ((i :> Dom.node Js.t) ## parentNode) with
        | None -> ()
        | Some p -> ignore (p ## removeChild ((i :> Dom.node Js.t)))


  let input_enabled = ref (S.const ())
  let set_input_enabled enabled =
    let enabler enabled =
      if enabled
      then start_text_input ()
      else stop_text_input ()
    in
    S.stop !input_enabled;
    input_enabled := S.map enabler enabled;
    ()

  (* That could be an option http://www.w3.org/TR/clipboard-apis/.
     But except for the result of set_clipboard_setter it seems hard
     to be able to provide Useri's interface on top of that. *)

  let clipboard : string signal = fst (S.create "")
  let set_clipboard_setter : string event -> unit =
    fun e -> invalid_arg "Unsupported in this backend"
end

(* Drop *)

module Drop = struct

  type file = Useri_base.Drop.file

  module File = struct
    (* Using http://www.w3.org/TR/FileAPI/ *)

    type t = file
    let inj, proj = Useri_base.Drop.File.create ()

    let to_js f : File.file Js.t = match proj f with
    | None -> invalid_arg err_not_jsoo_file
    | Some f -> f

    let path f = Js.to_string ((to_js f) ## name)
    let prepare f k =
      let f_js = to_js f in
      let r = jsnew File.fileReader () in
      let onload _ =
        let name = Js.to_string (f_js ## name) in
        let content =
          match Js.Opt.to_option (File.CoerceTo.string (r ## result)) with
          | None -> assert false
          | Some str -> Js.to_string str
        in
        Sys_js.register_file ~name ~content;
        k f (`Ok ());
        Js._false
      in
      let onerror _ =
        let err = string_of_int (r ## error ## code) in
        k f (`Error err); Js._false
      in
      r ## onload <- Dom.handler onload;
      r ## onerror <- Dom.handler onerror;
      r ## readAsBinaryString (f_js);
      ()
  end

  let file, send_file = E.create ()


  let drop _ e =
    Dom.preventDefault e;
    let files = e ## dataTransfer ## files in
    for i = 0 to files ## length - 1 do
      match Js.Opt.to_option (files ## item(i)) with
      | None -> assert false
      | Some file -> send_file (File.inj file);
    done;
    false

  let dd_support () =
    let d = Dom_html.(createDiv document) in
    Js.Optdef.test ((Js.Unsafe.coerce d) ## ondragenter) &&
    Js.Optdef.test ((Js.Unsafe.coerce d) ## ondragover) &&
    Js.Optdef.test ((Js.Unsafe.coerce d) ## ondrop)

  let init () =
    if not (dd_support ()) then warn_drag () else
    let stop _ e = Dom.preventDefault e; false in
    Ev.cb Dom_html.window Dom_html.Event.dragenter stop;
    Ev.cb Dom_html.window Dom_html.Event.dragover stop;
    Ev.cb Dom_html.window Dom_html.Event.drop drop

end

(* Human *)

module Human = struct
  let noticed = Useri_base.Human.noticed
  let interrupted = Useri_base.Human.interrupted
  let left = Useri_base.Human.left

  let rec feel_action feel set_feel () =
    let new_feel, delay = match S.value feel with
    | `Interacting -> `Interrupted, left -. interrupted
    | `Interrupted -> `Left, 0.
    | `Left -> assert false
    in
    set_feel new_feel;
    if delay = 0. then () else
    let action = feel_action feel set_feel in
    let ms = delay *. 1000. in
    ignore (Dom_html.window ## setTimeout (Js.wrap_callback action, ms));
    ()

  let feel () =
    let feel, set_feel = S.create `Interacting in
    let action = feel_action feel set_feel in
    let ms = interrupted *. 1000. in
    ignore (Dom_html.window ## setTimeout (Js.wrap_callback action, ms));
    feel

  let touch_target_size = Useri_base.Human.touch_target_size
  let touch_target_size_min = Useri_base.Human.touch_target_size_min
  let touch_target_pad = Useri_base.Human.touch_target_pad
  let average_finger_width = Useri_base.Human.average_finger_width
end

(* App *)

module App = struct

  let prefs_path ~org ~app = failwith "TODO"
  let size : size2 signal = fst (S.create Size2.zero)
  let pos : p2 signal = fst (S.create P2.o)
  let env key ~default parse =
    let args = match Url.Current.get () with
    | None -> []
    | Some (Url.Http u | Url.Https u) -> u.Url.hu_arguments
    | Some (Url.File u) -> u.Url.fu_arguments
    in
    try parse (List.assoc key args) with
    | _ -> default

  type mode = Useri_base.App.mode
  let mode_switch ?(init = `Windowed) e =
    let switch_mode = function
    | `Windowed -> `Fullscreen
    | `Fullscreen -> `Windowed
    in
    S.accum (E.map (fun _ m -> switch_mode m) e) init

  let mode_sig, set_mode_sig = S.create (S.const `Windowed)
  let (mode : mode signal) = S.switch ~eq:( == ) mode_sig

  let quit, send_quit = E.create ()

  (* Event and signal sinks *)

  type sink = Esink : 'a event -> sink | Ssink : 'a signal -> sink
  let sinks = ref []
  let sink_event e = sinks := Esink e :: !sinks
  let sink_signal s = sinks := Ssink s :: !sinks
  let release_sinks () =
    let release = function
    | Esink e -> E.stop ~strong:true e
    | Ssink s -> S.stop ~strong:true s
    in
    List.iter release !sinks; sinks := []

  (* Init, run and release *)

  let start, send_start = E.create ()
  let stop, send_stop = E.create ()
  let running = ref false
  let send_start ?step () =
    if not !running then (running := true; send_start ?step ())

  let send_stop ~step () = send_stop ~step (); running := false

  let init ?(hidpi = true) ?pos ?(size = V2.v 600. 400.)
      ?(name = String.capitalize execname)
      ?(surface = (`Gl Surface.Gl.default))
      ?anchor
      ?(mode = S.value mode_sig) ()
    =
    let send_quit _ _ = send_quit (); false in
    Ev.cb Dom_html.window Dom_html.Event.unload send_quit;
    let step = React.Step.create () in
    Key.init step;
    Surface.init ~hidpi ?pos ~size ~surface ?anchor ~mode ();
    Drop.init ();
    React.Step.execute step;
    `Ok ()

  let run_step () = send_start (); max_float
  let run ?(until = E.never) () = send_start ()
  let release ?(sinks = true) () =
    let step = Step.create () in
    send_stop ~step ();
    Key.release ~step;
    Step.execute step;
    Useri_base.App.(set_backend_logger default_backend_logger);
    if sinks then release_sinks ();
    Ev.release ();
    ()

  (* Launch context *)

  type launch_context = Useri_base.App.launch_context
  let launch_context = `Browser
  let pp_launch_context = Useri_base.App.pp_launch_context

  (* Platform and backend *)

  let platform = Js.to_string (Dom_html.window ## navigator ## platform)

  type backend = Useri_base.App.backend
  let backend = `Jsoo
  let pp_backend = Useri_base.App.pp_backend
  let set_backend_logger = Useri_base.App.set_backend_logger

  type backend_scheme = Useri_base.App.backend_scheme
  let backend_scheme = `Async
  let pp_backend_scheme = Useri_base.App.pp_backend_scheme

  (* CPU count *)

  type cpu_count = Useri_base.App.cpu_count
  let cpu_count =
    let n = Dom_html.window ## navigator in
    match Js.Optdef.to_option ((Js.Unsafe.coerce n) ## hardwareConcurrency)
    with None -> `Unknown | Some c -> `Known c

  let pp_cpu_count = Useri_base.App.pp_cpu_count
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
