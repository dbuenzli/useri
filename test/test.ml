(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React
open Useri
open Result

let log_formatter = ref Format.std_formatter
let log fmt = Format.fprintf !log_formatter (fmt ^^ "@\n%!")
let pp ppf = Format.fprintf ppf
let pp_str = Format.pp_print_string
let pp_str_esc ppf = pp ppf "%S"
let pp_bool = Format.pp_print_bool
let pp_float ppf v = pp ppf "%g" v
let pp_unit ppf () = pp ppf "()"
let pp_opt pp_v ppf = function
| None -> pp ppf "None"
| Some v -> pp ppf "Some %a" pp_v v

let trace_v pp mname vname v =
  log "@[%s.%s = %a@]" mname vname pp v

let trace_e pp mname ename e =
  let log occ = log "@[%s.%s ! %a@]" mname ename pp occ in
  App.sink_event (E.trace log e)

let trace_s pp mname sname s =
  let log v = log "@[%s.%s =@ %a@]" mname sname pp v in
  App.sink_signal (S.trace log s)

(* Tests setup by module *)

let test_time () =
  let mname = "Time" in
  let log_tick c v =
    log "Tick ! spanned:%a counter:%a"
      Time.pp_s v Time.pp_s (Time.counter_value c)
  in
  let t_down_count =
    let eq = ( == ) in
    let key = Key.uchar 't' in
    let on_down () = Time.count ~until:(Key.up key) in
    let t_holds = S.hold ~eq (S.const 0.) (E.map on_down (Key.down key)) in
    S.switch ~eq t_holds
  in
  let unit _ = trace_s pp_float "Time" "unit" (Time.unit ~span:0.3) in
  trace_s Time.pp_s mname "t_down_count" t_down_count;
  App.sink_event (E.map unit (Time.tick 1.1));
  App.sink_event (E.map (log_tick (Time.counter ())) (Time.tick 1.));
  log "%s.elapsed () = %a" mname Time.pp_s (Time.elapsed ());
  ()

let test_surface () =
  let mname = "Surface" in
  let pp_refresh ppf v =
    pp ppf "%a (abs: %a)" Time.pp_s v Time.pp_s (Time.elapsed ())
  in
  Surface.(set_mode_setter (mode_flip (Key.(up (uchar 'f')))));
  trace_s Surface.pp_mode mname "mode" Surface.mode;
  trace_s V2.pp mname "raster_size" Surface.raster_size;
  trace_s V2.pp mname "size" Surface.size;
  trace_s V2.pp mname "pos" Surface.pos;
  trace_e pp_refresh mname "refresh" Surface.refresh;
  let refresher = E.select [ Time.tick 1.5; Time.tick 1.6 ] in
  Surface.set_refresher refresher;
  App.sink_event (E.map (fun _ -> Surface.request_refresh ()) (Time.tick 1.7));
  ()

let test_mouse () =
  let mname = "Mouse" in
  trace_s V2.pp mname "pos" Mouse.pos;
  trace_e V2.pp mname "dpos" Mouse.dpos;
  trace_s pp_bool mname "left" Mouse.left;
  trace_e V2.pp mname "left_down" Mouse.left_down;
  trace_e V2.pp mname "left_up" Mouse.left_up;
  trace_s pp_bool mname "middle" Mouse.middle;
  trace_e V2.pp mname "middle_down" Mouse.middle_down;
  trace_e V2.pp mname "middle_up" Mouse.middle_up;
  trace_s pp_bool mname "right" Mouse.right;
  trace_e V2.pp mname "right_down" Mouse.right_down;
  trace_e V2.pp mname "right_up" Mouse.right_up;
  ()

let test_touch () =
  let mname = "Touch" in
  let trace_touch t =
    (* FIXME leaks *)
    let touch_fun f =
      Printf.sprintf "%s did:%d tid:%d" f (Touch.did t) (Touch.id t)
    in
    let pp_over ppf o =
      pp ppf (match o with `Up -> "`Up" | `Cancel -> "`Cancel")
    in
    trace_s V2.pp mname (touch_fun "pos") (Touch.pos t);
    trace_e V2.pp mname (touch_fun "dpos") (Touch.dpos t);
    trace_s pp_float mname (touch_fun "pressure") (Touch.pressure t);
    trace_e pp_over mname (touch_fun "over") (Touch.over t);
  in
  let pp_touch ppf t = pp ppf "did:%d tid:%d@ " (Touch.did t) (Touch.id t) in
  let pp_touches ppf ts =
    pp ppf "@[%a@]" (fun ppf ts -> List.iter (pp_touch ppf) ts) ts
  in
  trace_e pp_touches mname "start" Touch.start;
  App.sink_event (E.map (fun ts -> List.iter trace_touch ts) Touch.start);
  ()

let test_key () =
  let mname = "Key" in
  trace_e Key.pp_id mname "any_down" Key.any_down;
  trace_e Key.pp_id mname "any_up" Key.any_up;
  trace_s pp_bool mname "any_holds" Key.any_holds;
  trace_e pp_unit mname "down (`Digit 1)" (Key.down (`Digit 1));
  trace_s pp_bool mname "holds (`Digit 1)" (Key.holds (`Digit 1));
  trace_e pp_unit mname "up (`Digit 1)" (Key.up (`Digit 1));
  trace_s pp_bool mname "alt" Key.alt;
  trace_s pp_bool mname "ctrl" Key.ctrl;
  trace_s pp_bool mname "meta" Key.meta;
  trace_s pp_bool mname "shift" Key.shift;
  ()

let test_text set_clipboard  =
  if Useri.App.backend = `Jsoo then ((* API unsupported at the moment. *)) else
  let mname = "Text" in
  let pp_editing ppf (s, pos, l) = pp ppf "(%S,%d,%d)" s pos l in
  Text.set_input_enabled (S.const true);
  trace_e pp_str mname "input" Text.input;
  trace_e pp_editing mname "editing" Text.editing;
  trace_s pp_str_esc mname "clipboard" Text.clipboard;
  begin match set_clipboard with
  | Some s ->
      let cset, send_cset = E.create () in
      Text.set_clipboard_setter cset;
      log "Setting clipboard to %S" s;
      send_cset s;
  | None -> ()
  end;
  ()

let test_drop () =
  let mname = "Drop" in
  let pp_file ppf f = pp ppf "%s" (Drop.File.path f) in
  let read file state =
    let name = Drop.File.path file in
    match state with
    | Error (`Msg m) -> log "%s: file prepare error (%s)" name m
    | Ok () ->
        try
          if Sys.is_directory name then log "%s: directory" name else
          let ic = open_in name in
          let len = in_channel_length ic in
          let s = String.create len in
          really_input ic s 0 len; close_in ic;
          log "%s: contents: %S" name s
        with Sys_error e -> log "%s: %s" name e
  in
  trace_e pp_file mname "file" Drop.file;
  App.sink_event (E.map (fun f -> Drop.File.prepare f read) Drop.file);
  ()

let test_human () =
  let mname = "Human" in
  let pp_feel c ppf = function
  | `Interacting -> pp ppf "Interacting"
  | `Interrupted -> pp ppf "Interrupted (%a)" Time.pp_s (Time.counter_value c)
  | `Left -> pp ppf "Left (%a)" Time.pp_s (Time.counter_value c)
  in
  trace_s (pp_feel (Time.counter ())) mname "feel ()" (Human.feel ());
  ()

let test_app () =
  let mname = "App" in
  log "App.env: TEST=%S" (App.env "TEST" ~default:"" (fun v -> v));
  trace_e pp_unit mname "start" App.start;
  trace_e pp_unit mname "stop" App.stop;
  trace_v pp_str mname "platform" App.platform;
  trace_v App.pp_launch_context mname "launch_context" App.launch_context;
  trace_v App.pp_backend mname "backend" App.backend;
  trace_v App.pp_backend_scheme mname "backend_scheme" App.backend_scheme;
  trace_v App.pp_cpu_count mname "cpu_count" App.cpu_count;
  ()

let test mods set_clipboard =
  let do_test m = mods = [] || List.mem m mods in
  if do_test `Mouse then test_mouse ();
  if do_test `Touch then test_touch ();
  if do_test `Key then test_key ();
  if do_test `Text then test_text set_clipboard;
  if do_test `Drop then test_drop ();
  if do_test `Time then test_time ();
  if do_test `Human then test_human ();
  if do_test `Surface then test_surface ();
  if do_test `App then test_app ();
  ()

(* Tests selection and setup through Useri.App.env *)

let env_setup () =
  let env key v acc =
    let add s = if bool_of_string s then v :: acc else acc in
    Useri.App.env key ~default:acc add
  in
  let mods =
    env "mouse" `Mouse @@
    env "touch" `Touch @@
    env "key" `Key @@
    env "text" `Text @@
    env "drop" `Drop @@
    env "time" `Time @@
    env "human" `Human @@
    env "surface" `Surface @@
    env "app" `App @@
    []
  in
  let set_clipboard =
    Useri.App.env "set-clipboard" ~default:None (fun s -> Some s)
  in
  test mods set_clipboard

(* Test selection and setup through the command line. *)

let cmdline_setup () =
  let usage = Printf.sprintf
    "Usage: %s [OPTION]...\n\
     \ Traces events and signal, without any options tests everything.\n\
     Option:"  (Filename.basename Sys.executable_name)
  in
  let mods = ref [] in
  let set_clipboard = ref None in
  let add v = Arg.Unit (fun () -> mods := v :: !mods) in
  let pos p = raise (Arg.Bad ("don't know what to to with " ^ p)) in
  let options = [
    "-mouse", add `Mouse, " test the Mouse module";
    "-touch", add `Touch, " test the Touch module";
    "-key", add `Key, " test the Key module";
    "-text", add `Text, " test the Text module";
    "-drop", add `Drop, " test the Drop module";
    "-time", add `Time, " test the Time module";
    "-human", add `Time, " test the Human module";
    "-surface", add `Surface,  " test the Sufrace module";
    "-set-clipboard", Arg.String (fun s -> set_clipboard := Some s),
    "MSG set clipboard content to MSG.";
  ]
  in
  Arg.parse (Arg.align options) pos usage;
  test !mods !set_clipboard

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
