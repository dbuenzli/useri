(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Backend support. *)

open Gg
open React

(** Time. *)
module Time : sig
  type span = float
  val pp_s : Format.formatter -> span -> unit
  val pp_ms : Format.formatter -> span -> unit
  val pp_mus : Format.formatter -> span -> unit
end

(** Surface. *)
module Surface : sig

  type mode = [ `Windowed | `Fullscreen ]
  val mode_flip : mode -> mode
  val pp_mode : Format.formatter -> mode -> unit

  type handle

  module Handle : sig
    type t = handle
    val create : unit -> ('a -> t) * (t -> 'a option)
  end

  module Gl : sig
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
    val default : t
  end

  type kind = [ `Gl of Gl.t | `Other ]

  val default_size : size2

end

(** User keyboard. *)
module Key : sig

  (** {1 Key identifiers} *)

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
    | `Uchar of int
    | `Unknown of int ]

  val uchar : char -> [> `Uchar of int ]
  val pp_id : Format.formatter -> id -> unit

  (** {1 Key events and signals} *)

  val any_down : id event
  val any_up : id event
  val any_holds : bool signal
  val down : id -> unit event
  val up : id -> unit event
  val holds : id -> bool signal
  val alt : bool signal
  val ctrl : bool signal
  val meta : bool signal
  val shift : bool signal

  (** {1 Backend driving function} *)

  val init : step:Step.t -> unit
  val release : step:Step.t -> unit
  val handle_down : step:Step.t -> id -> unit
  val handle_up : step:Step.t -> id -> unit
end

(** User drag and drop. *)
module Drop : sig
  type file
  module File : sig
    type t = file
    val create : unit -> ('a -> t) * (t -> 'a option)
  end
end

(** Human factors. *)
module Human : sig
  val noticed : Time.span
  val interrupted : Time.span
  val left : Time.span
  val touch_target_size : float
  val touch_target_size_min : float
  val touch_target_pad : float
  val average_finger_width : float
end

(** Application  *)
module App : sig

  val default_name : string

  type launch_context = [ `Browser | `Gui | `Terminal ]
  val pp_launch_context : Format.formatter -> launch_context -> unit

  type backend = [ `Tsdl | `Jsoo | `Other of string ]
  val pp_backend : Format.formatter -> backend -> unit

  type backend_scheme = [ `Sync | `Async ]
  val pp_backend_scheme : Format.formatter -> backend_scheme -> unit

  val default_backend_logger : [`Error | `Warning ] -> string -> unit
  val set_backend_logger : ([`Error | `Warning ] -> string -> unit) -> unit
  val backend_log : [`Error | `Warning ] -> string -> unit

  type cpu_count = [ `Known of int | `Unknown ]
  val pp_cpu_count : Format.formatter -> cpu_count -> unit
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
