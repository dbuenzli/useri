(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Traces events and signals.

   Use the query string to enable and disable modules, e.g. add
   ?key=true to only get Key module events and signals. *)

open Gg
open React
open Useri

let setup_log () =
  let log = Dom_html.(createDiv document) in
  let add_entry s =
    let e = Dom_html.(createP document) in
    e ## innerHTML <- Js.string s;
    Dom.insertBefore log e (log ## firstChild)
  in
  Dom.appendChild (Dom_html.document ## body) log;
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
  | `Error e -> Printf.eprintf "%s" e; exit 1
  | `Ok () ->
      setup_log ();
      Test.env_setup ();
      App.run ();
      App.sink_event (E.map App.release App.quit)

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
