(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
