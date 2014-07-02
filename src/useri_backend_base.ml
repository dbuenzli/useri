(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React

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
    | `Alt dir -> Format.fprintf ppf "alt_%s" (dir_to_string dir)
    | `Arrow dir -> Format.fprintf ppf "arrow_%s" (dir_to_string dir)
    | `Backspace -> Format.fprintf ppf "backspace"
    | `Ctrl dir -> Format.fprintf ppf "ctrl_%s" (dir_to_string dir)
    | `Digit d -> 
        if uchar then Format.fprintf ppf "%d" d else
        Format.fprintf ppf "digit_%d" d
    | `End -> Format.fprintf ppf "end"
    | `Enter -> Format.fprintf ppf "enter"
    | `Escape -> Format.fprintf ppf "escape" 
    | `Function n -> Format.fprintf ppf "f%d" n
    | `Home -> Format.fprintf ppf "home" 
    | `Meta dir -> Format.fprintf ppf "meta_%s" (dir_to_string dir)
    | `Page dir -> Format.fprintf ppf "page_%s" (dir_to_string dir)
    | `Return -> Format.fprintf ppf "return"
    | `Shift dir -> Format.fprintf ppf "shift_%s" (dir_to_string dir)
    | `Space -> Format.fprintf ppf "space"
    | `Tab -> Format.fprintf ppf "tab" 
    | `Uchar u -> Format.fprintf ppf "U+%04X" u
    | `Unknown u -> Format.fprintf ppf "unknown (%X)" u 
    end
end

(* Time *) 

module Time = struct 
  type span = float 
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

module App = struct
 
  type launch_context = [ `Browser | `Gui | `Terminal ]                        
  let pp_launch_context ppf lc = Format.fprintf ppf begin match lc with 
    | `Browser -> "browser"
    | `Gui -> "gui" 
    | `Terminal -> "terminal" 
    end

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
