(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** js_of_ocaml specifics *)

(** User keyboard.

    Consult information about {{!get}getting keyboard events} and
    {{!limits}limitations}.
*)
module Key : sig

(** {1:src Keyboard event target} *)

  val event_target : unit -> Dom_html.eventTarget Js.t option
  (** [source ()] is the event target used for keyboard events. *)

  val set_event_target : Dom_html.eventTarget Js.t option -> unit
  (** [set_event_target target] sets the event target to target. If [None]
      the canvas of the surface will be used.

      {b Important.} You need to set the event target before
      calling {!Useri.App.init}. And a {!Useri.App.release} sets
      the event target back to [None]. *)

(** {1:get Getting keyboard events}

    There are a few things you need to make sure are setup in order
    to get the keyboard events.

    First to get events directly on the HTML canvas associated to the
    surface it needs to have a [tabindex] attribute. If [Useri]
    creates the canvas it sets one, but don't forget to set one if you
    provide the canvas at initialization time through an
    {{!Surface.anchor_of_canvas}anchor}. Events will only be generated
    once the user has focused the canvas in one way or another
    (e.g. by clicking on it). The latter operation may introduce a
    selection box around the canvas, the selection box can be hidden
    by applying the CSS rule [{ outline: none; }] on the canvas.

    You can also choose to get the keyboard events from another event
    target using the {!set_event_target} before initializing the
    application. For example using the {!Dom_html.window} will prevent
    the user from having to focus in order for you to get keyboard
    events.

    {1:limits Limitations}

    The following limitations exist (they may be lifted in the future).
    {ul
    {- The backend cannot distinguish between [`Enter] and
      [`Return] keys. [`Return] is always returned.}
    {- The backend cannot distinguish between [`Left] and [`Right] keys for
       modifiers [`Alt, `Ctrl, `Meta] and [`Shift]. It is advised to use
       handless {!Useri.Key.alt}, {!Useri.Key.ctrl}, {!Useri.Key.meta} and
       {!Useri.Key.shift} for detecting modifiers.}}
*)
end


module Surface : sig

  (** {1:anchor Anchors} *)

  val anchor_of_canvas : Dom_html.canvasElement Js.t -> Useri_base.anchor
  val canvas_of_anchor : Useri_base.anchor -> Dom_html.canvasElement Js.t
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
