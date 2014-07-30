(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React
open Useri

let redirect_logs () = match Useri.App.launch_context with
| `Terminal -> ()
| `Gui ->
    let backend_log kind msg = match kind with
    | `Warning -> Tsdl.(Sdl.log_warn Sdl.Log.category_application "%s" msg)
    | `Error -> Tsdl.(Sdl.log_error Sdl.Log.category_application "%s" msg)
    in
    let ppf =
      let b = Buffer.create 255 in
      let out = Buffer.add_substring b in
      let flush () = Tsdl.Sdl.log "%s" (Buffer.contents b); Buffer.clear b in
      Format.make_formatter out flush
    in
    Useri.App.set_backend_logger backend_log;
    Test.log_formatter := ppf;
    ()
| `Browser -> assert false

let test_setup () = match Useri.App.launch_context with
| `Terminal -> Test.cmdline_setup ()
| `Gui -> Test.env_setup ()
| `Browser -> assert false

let main () =
  redirect_logs ();
  let hidpi = App.env "HIDPI" ~default:true bool_of_string in
  let mode = App.mode_switch ~init:`Windowed (Key.up `Space) in
  let size = Size2.v 600. 400. in
  match App.init ~hidpi ~size ~mode () with
  | `Error e -> Test.log "%s" e; exit 1
  | `Ok () ->
      test_setup ();
      App.run ~until:App.quit ();
      App.release ();
      exit 0

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
