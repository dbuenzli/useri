(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(**
   * tsdl backend, compile with:
     ocamlfind ocamlopt -package tsdl,useri.tsdl,useri \
                        -linkpgk -o min.native min.ml

   * js_of_ocaml backend, compile with:
     ocamlfind ocamlc -package useri.jsoo, useri \
                      -linkpkg -o min.byte min.ml \
     && js_of_ocaml min.byte
  *)

open Gg
open React
open Useri
open Result

let setup size =
  let s = S.value size in
  let resize = S.l1 (Renderer.set_size r) size in
  let draw = E.map (render r) Surface.refresh in
  App.sink_event draw;
  App.sink_signal resize;
  Surface.steady_refresh ~until:E.never

let main () =
  let mode = Surface.mode_switch ~init:`Windowed (Key.up `Space) in
  let surface = Surface.create ~mode in
  match App.init ~surface () with
  | Error e -> Printf.eprintf "%s" e; exit 1
  | Ok () ->
      App.run ~until:App.quit ();
      match App.backend_scheme with
      | `Sync -> App.release (); exit 0
      | `Async -> App.sink_event (E.map App.release App.quit)

let () = main ()

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
