(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React
open Useri

let log fmt = Format.printf (fmt ^^ "@\n%!") 
let pp fmt = Format.fprintf fmt
let pp_str = Format.pp_print_string
let pp_str_esc ppf = pp ppf "%S" 
let pp_bool = Format.pp_print_bool
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

let trace_mouse () = 
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

let trace_key () = 
  let mname = "Key" in
  trace_e Key.pp_sym mname "any_down" Key.any_down; 
  trace_e Key.pp_sym mname "any_repeat" Key.any_repeat;
  trace_e Key.pp_sym mname "any_up" Key.any_up;
  trace_s pp_bool mname "any_holds" Key.any_holds;
  trace_s pp_bool mname "meta" Key.meta; 
  trace_s pp_bool mname "ctrl" Key.ctrl; 
  trace_s pp_bool mname "alt" Key.alt; 
  ()
  
let trace_text set_clipboard = 
  let mname = "Text" in
  Text.set_input_enabled true;
  trace_s pp_bool mname "input_enabled" Text.input_enabled;
  trace_e pp_str mname "input" Text.input; 
  trace_e (fun ppf (s,pos,l) -> pp ppf "(%S,%d,%d)" s pos l) mname "editing" 
    Text.editing;
  trace_s (pp_opt pp_str_esc) mname "clipboard" Text.clipboard;
  match set_clipboard with 
  | Some s as c -> log "Setting clipboard to %S" s; Text.set_clipboard c
  | None -> ()
  
let trace_app () = 
  let mname = "App" in 
  trace_e pp_unit mname "start" App.start;
  trace_e pp_unit mname "stop" App.stop;
  trace_v pp_str mname "platform" App.platform;
  trace_v App.pp_launch_context mname "launch_context" App.launch_context;
  trace_v App.pp_backend mname "backend" App.backend;
  trace_v App.pp_backend_scheme mname "backend_scheme" App.backend_scheme;
  trace_v App.pp_cpu_count mname "cpu_count" App.cpu_count;
  ()

let test mods set_clipboard = 
  let do_trace m = mods = [] || List.mem m mods in
  if do_trace `Mouse then trace_mouse ();
  if do_trace `Key   then trace_key (); 
  if do_trace `Text  then trace_text set_clipboard; 
  if do_trace `App   then trace_app ();
  ()
  
let parse_args_and_setup () =
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
    "-mouse", add `Mouse, " trace the Mouse module"; 
    "-key",   add `Key,   " trace the Key module"; 
    "-text",  add `Text,  " trace the Text module";
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
