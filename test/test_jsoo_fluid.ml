(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Traces events and signals.

   Use the query string to enable and disable modules, e.g. add
   ?key=true to only get Key module events and signals. *)

open Js_of_ocaml
open Gg
open React
open Useri

let setup_log () =
  let log = Dom_html.(createDiv document) in
  let add_entry s =
    let e = Dom_html.(createP document) in
    e ##. innerHTML := Js.string s;
    Dom.insertBefore log e (log ##. firstChild)
  in
  Dom.appendChild (Dom_html.document ##. body) log;
  Sys_js.set_channel_flusher stdout add_entry;
  Sys_js.set_channel_flusher stderr add_entry;
  ()

let setup_jsoo global_key =
  let target = (Dom_html.window :> Dom_html.eventTarget Js.t) in
  if global_key then Useri_jsoo.Key.set_event_target (Some target);
  ()

let main () =
  let global_key = App.env "global-key" ~default:false bool_of_string in
  let hidpi = App.env "HIDPI" ~default:true bool_of_string in
  let surface = Surface.create ~hidpi ~kind:`Other () in
  setup_jsoo global_key;
  match App.init ~surface () with
  | Error (`Msg m) -> Printf.eprintf "%s" m; exit 1
  | Ok () ->
      setup_log ();
      Test.env_setup ();
      App.run ();
      App.sink_event (E.map App.release App.quit)

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
