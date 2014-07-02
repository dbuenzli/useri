(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React

module Mouse = struct
  let pos : p2 signal = fst (S.create P2.o)
  let dpos : v2 event = fst (E.create ())
  let left : bool signal = fst (S.create false)
  let left_down : p2 event = fst (E.create ())
  let left_up : p2 event = fst (E.create ())
  let middle : bool signal = fst (S.create false)
  let middle_down : p2 event = fst (E.create ())
  let middle_up : p2 event = fst (E.create ())
  let right : bool signal = fst (S.create false)
  let right_up : p2 event = fst (E.create ())
  let right_down : p2 event = fst (E.create ())
end

module Key = struct
  include Useri_base.Key 

  let any_down : sym event = fst (E.create ())
  let any_repeat : sym event = fst (E.create ())
  let any_up : sym event = fst (E.create ())
  let any_holds : bool signal = fst (S.create false)
  let down : ?repeat:bool -> sym -> unit event = 
    fun ?(repeat = false) sym -> failwith "TODO"

  let up : sym -> unit event = fun sym -> failwith "TODO"
  let holds : sym -> bool signal = fun sym -> failwith "TODO"
  let meta : bool signal = fst (S.create false)
  let ctrl : bool signal = fst (S.create false) 
  let alt : bool signal = fst (S.create false)
end

module Text = struct 
  let input_enabled : bool signal = fst (S.create false)
  let set_input_enabled : bool -> unit = fun _ -> ()
  let input : string event = fst (E.create ())
  let editing : (string * int * int) event = fst (E.create ())
  let clipboard : string option signal = fst (S.create None)
  let set_clipboard : string option -> unit = fun s -> failwith "TODO"
end

module Drop = struct
  let file : string event = fst (E.create ())
end

module Time = struct
  include Useri_base.Time

  let elapsed : unit -> span = fun () -> failwith "TODO"
  let tick : span -> span event = fun span -> failwith "TODO"
  type counter = unit
  let counter : unit -> counter = fun () -> ()
  let value : counter -> span = fun () -> failwith "TODO"
end

module Human = struct
  include Useri_base.Human
  let feel : unit -> [ `Interacting | `Interrupted | `Left ] signal = 
    fun () -> failwith "TODO"
end

module Surface = struct

  let size : size2 signal = fst (S.create Size2.zero)
  let update : unit -> unit = fun () -> failwith "TODO"
  let refresh : float event = fst (E.create ())
  let request_refresh : unit -> unit = fun () -> failwith "TODO"
  let set_refresher : 'a event -> unit = fun e -> failwith "TODO"
  let steady_refresh : until:'a event -> unit = fun ~until -> failwith "TODO"
  let animate : span:float -> float signal = fun ~span -> failwith "TODO"
  let refresh_hz : int signal = fst (S.create 60)
  let set_refresh_hz : int -> unit = fun hz -> ()

  type colors = [ `RGBA_8888 | `RGB_565 ]
  type depth = [ `D_24 | `D_16 ]
  type stencil = [ `S_8 ]
  type spec = unit

  let spec 
      ?share
      ?accelerated
      ?multisample
      ?doublebuffer
      ?stereo
      ?srgb
      ?colors
      ?depth
      ?stencil
      ~gl:(min, maj) () = ()
                    

end

module App = struct
  include Useri_base.App

  let launch_context = `Browser
  let platform = "TODO"
  let backend_runtime = `Async
  let cpu_count = 0 (* TODO *)
  let prefs_path ~org ~app = failwith "TODO"
  let size : size2 signal = fst (S.create Size2.zero)
  let pos : p2 signal = fst (S.create P2.o)
  let env key ~default parse = failwith "TODO"

  let mode : mode signal = fst (S.create `Windowed)
  let mode_switch ?init e = failwith "TODO"
  let quit : unit event = fst (E.create ())
  let sink_event : 'a event -> unit = fun e -> failwith "TODO"
  let sink_signal : 'a signal -> unit = fun s -> failwith "TODO"
  let clear_sinks : unit -> unit = failwith "TODO"
  let start : unit event = fst (E.create ())
  let stop : unit event = fst (E.create ())
  let init ?hidpi ?pos ?size ?name ?surface ?mode () = failwith "TODO"
  let run_step : unit -> Time.span = fun () -> failwith "TODO"
  let run : until:'a event -> unit = fun ~until -> failwith "TODO"
  let release : unit -> unit = fun () -> ()
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
