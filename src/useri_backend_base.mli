(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Base module for backends. *) 

open Gg
open React

(** User keyboard. *)
module Key : sig

  type sym =
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
  val pp_sym : ?uchar:bool -> Format.formatter -> sym -> unit
end

(** Time. *)
module Time : sig
  type span = float 
  val pp_s : Format.formatter -> span -> unit
  val pp_ms : Format.formatter -> span -> unit
  val pp_mus : Format.formatter -> span -> unit
end

(** Human factors. *) 
module Human : sig
  val noticed : Time.span
  val interrupted : Time.span
  val left : Time.span
  val feel : unit -> [ `Interacting | `Interrupted | `Left ] signal
  val touch_target_size : float 
  val touch_target_size_min : float 
  val touch_target_pad : float 
  val average_finger_width : float 
end

(** Application  *) 
module App : sig
  type launch_context = [ `Browser | `Gui | `Terminal ]
  val pp_launch_context : Format.formatter -> launch_context -> unit 
  type mode = [ `Windowed | `Fullscreen ] 
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
