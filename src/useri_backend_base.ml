(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React

let pp = Format.fprintf

(* Keyboard *) 

module Key = struct

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
    
  let uchar u = `Uchar (Char.code u)
                      
  let pp_sym ?(uchar = false) ppf ksym = 
    let dir_to_string = function 
    | `Left -> "left" | `Right -> "right" | `Up -> "up" | `Down -> "down"
    in
    begin match ksym with
    | `Alt dir -> pp ppf "alt_%s" (dir_to_string dir)
    | `Arrow dir -> pp ppf "arrow_%s" (dir_to_string dir)
    | `Backspace -> pp ppf "backspace"
    | `Ctrl dir -> pp ppf "ctrl_%s" (dir_to_string dir)
    | `Digit d -> 
        if uchar then pp ppf "%d" d else
        pp ppf "digit_%d" d
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
    | `Uchar u -> pp ppf "U+%04X" u
    | `Unknown u -> pp ppf "unknown (%X)" u 
    end
end

(* Time *) 

module Time = struct 
  type span = float 
  let pp_s ppf s = pp ppf "%gs" s
  let pp_ms ppf s = pp ppf "%gms" (s *. 1e3)
  let pp_mus ppf s = pp ppf "%gμs" (s *. 1e6)
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

(* Surface *) 

module Surface = struct
  module Gl = struct
    type colors = [ `RGBA_8888 | `RGB_565 ]
    type depth = [ `D_24 | `D_16 ]
    type stencil = [ `S_8 ]
    type spec = 
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

    type kind = [ `Gl of Gl.spec | `Other ]
end

(* Application *)

module App = struct

  type mode = [ `Windowed | `Fullscreen ]

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

  let invalid_on_backend_scheme bs = 
    invalid_arg (Format.asprintf "unapplicable in %a backend scheme" 
                   pp_backend_scheme bs)

  (* Cpu count *)

  type cpu_count = [ `Known of int | `Unknown ]
  let pp_cpu_count ppf = function 
  | `Unknown -> pp ppf "unknown"
  | `Known c -> pp ppf "%d" c                   
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
