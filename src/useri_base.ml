(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React

let pp = Format.fprintf

(* Universal type. See http://mlton.org/UniversalType *)

module Univ : sig
  type t
  val create : unit -> ('a -> t) * (t -> 'a option)
end = struct
  type t = exn
  let create (type s) () =
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)
end

(* Time *)

module Time = struct
  type span = float
  let pp_s ppf s = pp ppf "%gs" s
  let pp_ms ppf s = pp ppf "%gms" (s *. 1e3)
  let pp_mus ppf s = pp ppf "%gμs" (s *. 1e6)
end

(* Surface *)

module Surface = struct

  type mode = [ `Windowed | `Fullscreen ]
  let mode_flip = function `Windowed -> `Fullscreen | `Fullscreen -> `Windowed
  let pp_mode ppf m = pp ppf "%s" begin match m with
    | `Windowed -> "windowed"
    | `Fullscreen -> "fullscreen"
    end

  module Handle = struct
    type t = Univ.t
    let create = Univ.create
  end

  type handle = Handle.t

  module Gl = struct
    type colors = [ `RGBA_8888 | `RGB_565 ]
    type depth = [ `D_24 | `D_16 ]
    type stencil = [ `S_8 ]
    type t =
      { accelerated : bool option;
        multisample : int option;
        doublebuffer : bool;
        stereo : bool;
        srgb : bool;
        colors : colors;
        depth : depth option;
        stencil : stencil option;
        version : int * int; }

    let default =
      { accelerated = None;
        multisample = Some 8;
        doublebuffer = true;
        stereo = false;
        srgb = true;
        colors = `RGBA_8888;
        depth = Some `D_24;
        stencil = None;
        version = (3,2); }
  end

  type kind = [ `Gl of Gl.t | `Other ]

  let default_size = V2.v 600. 400.
end

(* Keyboard *)

module Key = struct

  (* Key identifiers *)

  type id =
    [ `Alt of [ `Left | `Right ]
    | `Arrow of [ `Up | `Down | `Left | `Right ]
    | `Backspace
    | `Ctrl of [ `Left | `Right ]
    | `Digit of int
    | `End
    | `Enter
    | `Escape
    | `Function of int
    | `Home
    | `Meta of [ `Left | `Right ]
    | `Page of [ `Up | `Down ]
    | `Return
    | `Shift of [ `Left | `Right ]
    | `Space
    | `Tab
    | `Uchar of Uchar.t
    | `Unknown of int ]

  let uchar u = `Uchar (Uchar.of_char u)

  let pp_id ppf id =
    let dir_to_string = function
    | `Left -> "left" | `Right -> "right" | `Up -> "up" | `Down -> "down"
    in
    begin match id with
    | `Alt dir -> pp ppf "alt_%s" (dir_to_string dir)
    | `Arrow dir -> pp ppf "arrow_%s" (dir_to_string dir)
    | `Backspace -> pp ppf "backspace"
    | `Ctrl dir -> pp ppf "ctrl_%s" (dir_to_string dir)
    | `Digit d -> pp ppf "digit_%d" d
    | `End -> pp ppf "end"
    | `Enter -> pp ppf "enter"
    | `Escape -> pp ppf "escape"
    | `Function n -> pp ppf "f%d" n
    | `Home -> pp ppf "home"
    | `Meta dir -> pp ppf "meta_%s" (dir_to_string dir)
    | `Page dir -> pp ppf "page_%s" (dir_to_string dir)
    | `Return -> pp ppf "return"
    | `Shift dir -> pp ppf "shift_%s" (dir_to_string dir)
    | `Space -> pp ppf "space"
    | `Tab -> pp ppf "tab"
    | `Uchar u -> pp ppf "%04X" (Uchar.to_int u)
    | `Unknown u -> pp ppf "unknown (%X)" u
    end

  (* Key events *)

  let any_down, send_any_down = E.create ()
  let any_up, send_any_up = E.create ()
  let down_count = ref 0
  let any_holds, set_any_holds = S.create false

  let down_event = Hashtbl.create 47
  let up_event = Hashtbl.create 47

  let def_event event id = try fst (Hashtbl.find event id) with
  | Not_found ->
      let def = E.create () in
      Hashtbl.add event id def;
      fst def

  let send_event ?step event id = try snd (Hashtbl.find event id) ?step ()
  with Not_found -> ()

  let up id = def_event up_event id
  let down id = def_event down_event id

  (* Key signals *)

  let state_signal = Hashtbl.create 47
  let set_signal ?step id v = try snd (Hashtbl.find state_signal id) ?step v
  with Not_found -> ()

  let holds id = try fst (Hashtbl.find state_signal id) with
  | Not_found ->
      let def = S.create false in
      Hashtbl.add state_signal id def;
      fst def

  (* Modifiers *)

  let lalt = S.create false
  let ralt = S.create false
  let alt = S.Bool.(fst lalt || fst ralt)

  let lctrl = S.create false
  let rctrl = S.create false
  let ctrl = S.Bool.(fst lctrl || fst rctrl)

  let lmeta = S.create false
  let rmeta = S.create false
  let meta = S.Bool.(fst lmeta || fst rmeta)

  let lshift = S.create false
  let rshift = S.create false
  let shift = S.Bool.(fst lshift || fst rshift)

  let add_modifiers () =
    Hashtbl.add state_signal (`Alt `Left) lalt;
    Hashtbl.add state_signal (`Alt `Right) ralt;
    Hashtbl.add state_signal (`Ctrl `Left) lctrl;
    Hashtbl.add state_signal (`Ctrl `Right) rctrl;
    Hashtbl.add state_signal (`Meta `Left) lmeta;
    Hashtbl.add state_signal (`Meta `Right) rmeta;
    Hashtbl.add state_signal (`Shift `Left) lshift;
    Hashtbl.add state_signal (`Shift `Right) rshift;
    ()

  (* Backend driving function *)

  let init ~step = add_modifiers ()
  let release ~step =
    down_count := 0;
    set_any_holds ~step false;
    (snd lalt) ~step false;
    (snd lctrl) ~step false;
    (snd lmeta) ~step false;
    (snd ralt) ~step false;
    (snd rctrl) ~step false;
    (snd rmeta) ~step false;
    (snd lshift) ~step false;
    (snd rshift) ~step false;
    Hashtbl.reset down_event;
    Hashtbl.reset up_event;
    Hashtbl.reset state_signal;
    ()

  let handle_down ~step id =
    incr down_count;
    send_any_down ~step id;
    set_any_holds ~step true;
    send_event ~step down_event id;
    set_signal ~step id true;
    ()

  let handle_up ~step id =
    decr down_count;
    send_any_up ~step id;
    if !down_count <= 0 then (down_count := 0; set_any_holds ~step false);
    send_event ~step up_event id;
    set_signal ~step id false;
    ()
end

(* Drop *)

module Drop = struct
  module File = struct
    type t = Univ.t
    let create = Univ.create
  end
  type file = File.t
end

(* Human *)

module Human = struct

  let noticed = 0.1
  let interrupted = 1.
  let left = 10.

  (* Sizes in mm. *)
  let touch_target_size = 9.
  let touch_target_size_min = 7.
  let touch_target_pad = 2.
  let average_finger_width = 11.
end

(* Application *)

module App = struct

  let default_name =
    let base = Filename.basename Sys.argv.(0) in
    let name =
      try Filename.chop_extension base with
      | Invalid_argument _ (* this API is pathetic *) -> base
    in
    String.capitalize_ascii name

  (* Launch context *)

  type launch_context = [ `Browser | `Gui | `Terminal ]
  let pp_launch_context ppf lc = pp ppf begin match lc with
    | `Browser -> "browser"
    | `Gui -> "gui"
    | `Terminal -> "terminal"
    end

  (* Backend *)

  type backend = [ `Tsdl | `Jsoo | `Other of string ]
  let pp_backend ppf b = pp ppf "%s" begin match b with
    | `Tsdl -> "tsdl"
    | `Jsoo -> "jsoo"
    | `Other b -> b
    end

  type backend_scheme = [ `Sync | `Async ]
  let pp_backend_scheme ppf b = pp ppf begin match b with
    | `Sync -> "sync"
    | `Async -> "async"
    end

  let default_backend_logger kind msg =
    let kind = match kind with `Error -> "E" | `Warning -> "W" in
    Printf.eprintf "Useri:%s: %s\n%!" kind msg

  let backend_logger = ref default_backend_logger
  let backend_log kind msg = !backend_logger kind msg
  let set_backend_logger l = backend_logger := l

  (* Cpu count *)

  type cpu_count = [ `Known of int | `Unknown ]
  let pp_cpu_count ppf = function
  | `Unknown -> pp ppf "unknown"
  | `Known c -> pp ppf "%d" c
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
