(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open React
open Useri
open Result

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
  let size = Size2.v 600. 400. in
  let surface = Surface.create ~hidpi ~size () in
  match App.init ~surface () with
  | Error (`Msg m) -> Test.log "%s" m; exit 1
  | Ok () ->
      test_setup ();
      App.run ~until:App.quit ();
      App.release ();
      exit 0

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
