(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Declarative user interaction with React.

    [Useri] gathers user input as {!React} signals and events. 

    Open the module to use it, this defines only modules in your scope.

    {b Note.} Before {!App.init} is called all signals hold invalid data. 

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

open Gg
open React

(** {1 Input} *)

(** User mouse.

    {b Note.} Coordinates are in window normalized coordinates with 
    (0,0) bot left and (1,1) top right. *)
module Mouse : sig

  (** {1 Mouse position} *)
  
  val pos : p2 signal
  (** [pos] is the current mouse position. *) 

  val dpos : v2 event
  (** [dpos] occurs on mouse moves with current mouse position minus
      the previous position. *)

  (** {1 Mouse buttons} *)

  val left : bool signal
  (** [left] is [true] whenever the left mouse button is down. *) 

  val left_down : p2 event
  (** [left_down] has an occurence whenever the left mouse button goes down. *) 

  val left_up : p2 event
  (** [left_up] has an occurence whenever the left mouse button goes up. *) 

  val middle : bool signal
  (** [middle] is [true] whenever the middle mouse button is down. *) 

  val middle_down : p2 event
  (** [middle_down] has an occurence whenever the middle mouse button goes 
      down. *)

  val middle_up : p2 event
  (** [middle_up] has an occurence whenever the middle mouse button goes 
      up. *) 

  val right : bool signal
  (** [right] is [true] whenever the right mouse button is down. *) 
  
  val right_up : p2 event
  (** [right_up] has an occurence whenever the right mouse button goes up. *) 

  val right_down : p2 event
  (** [right_down] has an occurence whenever the right mouse button goes 
      down. *)
end

(** User keyboard. *)
module Key : sig

  (** {1 Key symbols} *)

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

  val uchar : char -> [> `Uchar of int ]

  val pp_sym : ?uchar:bool -> Format.formatter -> sym -> unit
  (** [pp_sym ppf sym] prints an unspecified representation of [sym] on 
      [ppf]. If [char] is [true] prints character as UTF-8 sequences
      rather than using the U+XXXX notation. *)

  (** {1 Key events and signals} 

      {b TODO.} Clarify what happens if created during update step. *)

  val any_down : sym event 
  (** [any_down] occurs whenever a key is down. *)

  val any_repeat : sym event
  (** [sym_repeat] occurs whenever a key is down and continues to 
      occur periodically while a key is down. *)

  val any_up : sym event 
  (** [sym_up] occurs whenever a key is up. *) 

  val any_holds : bool signal 
  (** [any_holds] is [true] whenever any key down. *) 

  val down : ?repeat:bool -> sym -> unit event
  (** [down repeat sym] occurs whenever key [sym] is down.  If
      [repeat] is [true] (defaults to [false]) the event continues to 
      occur periodically while the key is down. *) 

  val up : sym -> unit event 
  (** [up sym] occurs whenever key [sym] is up. *) 

  val holds : sym -> bool signal
  (** [holds sym] is [true] whenever [sym] is down. *)

  (** {1 Key modifiers signals} *) 

  val meta : bool signal 
  (** [meta] is [true] whenever a meta key is down. Equivalent to:
      {[S.l2 ( || ) (holds (`Meta `Left)) (holds (`Meta `Right)]} *)

  val ctrl : bool signal 
  (** [ctrl] is [true] whenever a ctrl key is down. Equivalent to:
      {[S.l2 ( || ) (holds (`Ctrl `Left)) (holds (`Ctrl `Right)]} *)

  val alt : bool signal 
  (** [alt] is [true] whenever an alt key is down. Equivalent to: 
      {[S.l2 ( || ) (holds (`Alt `Left)) (holds (`Alt `Right)]} *)
end

(** User text keyboard input and clipboard. *)
module Text : sig 

  (** {1 Text keyboard input} *)

  val input_enabled : bool signal
  (** [input_enabled] is [true] if {!input} and {!editing} events 
      are enabled. *)

  val set_input_enabled : bool -> unit 
  (** [enable_input b] allows to get {!input} and {!editing} events. 
      
      {b TODO} Use event. *)
  
  val input : string event
  (** [input] is the text input. *)

  val editing : (string * int * int) event 
  (** [editing] is the edited string the start position and the length. *)

  (** {1 Clipboard} *)

  val clipboard : string option signal
  (** [clipboard] is the clipboard's textual contents. *)

  val set_clipboard : string option -> unit
  (** [set_clipboard s] sets the clipboard to [s]. 

      {b TODO} Use event. *)
end

(** User drag and drop. *)
module Drop : sig

  (** {1 Files} *)

  val file : string event
  (** [file] occurs whenever a file is dropped on the application. *)
end

(** Time. 

    [Time] gives access to a simple monotonic clock. *)
module Time : sig

  (** {1 Time span} *) 

  type span = float 
  (** The type for time spans, in seconds. *) 

  (** {1 Passing time} *) 

  val elapsed : unit -> span
  (** [elapsed ()] is the number of seconds elapsed since the
      beginning of the program. *)

  val tick : span -> span event 
  (** [tick span] is an event that occurs once in [span] seconds with
      the value [span - span'] where [span'] is the actual delay
      performed by the system. 

      {b Note.} Since the system may introduce delays you cannot 
      assume that two calls to {!tick} will necessarily yield two 
      non-simultaneous events. TODO should we do it only if they 
      have the exact same absolute schedule time ? *)

  (** {1 Counting time} *)

  type counter 
  (** The type for time counters. *) 
  
  val counter : unit -> counter
  (** [counter ()] is a counter counting time from call time on. *)

  val value : counter -> span
  (** [value c] is the current counter value in seconds. *) 

  (** {1 Pretty printing time} *) 

  val pp_s : Format.formatter -> span -> unit
  (** [pp_s ppf s] prints [s] seconds in seconds. *) 

  val pp_ms : Format.formatter -> span -> unit
  (** [pp_ms ppf s] prints [s] seconds in milliseconds. *) 

  val pp_mus : Format.formatter -> span -> unit
  (** [pp_mus ppf s] prints [s] seconds in microseconds. *)
end

(** Human factors. *) 
module Human : sig

  (** {1 System latency feelings} 

      These values are from 
      {{:http://www.nngroup.com/articles/response-times-3-important-limits/}
      here}. *) 

  val noticed : Time.span
  (** [noticed] is [0.1]s, the time span after which the user will
      notice a delay and feel that the system is not reacting
      instantaneously. *)

  val interrupted : Time.span
  (** [interrupted] is [1.]s, the time span after which the user will
      feel interrupted and feedback from the system is needed. *) 

  val left : Time.span
  (** [left] is [10.]s, the time span after which the user will
      switch to another task and feedback indicating when the system
      expects to respond is needed. *)

  val feel : unit -> [ `Interacting | `Interrupted | `Left ] signal
  (** [feel ()] is a signal that varies according to user latency
      constants:
      {ul 
      {- \[[user_feel ()]\]{_t} [= `Interacting] if 
         [t < User.interrupted].}
      {- \[[user_feel ()]\]{_t} [= `Interrupted] if 
         [t < User.left].}
      {- \[[user_feel ()]\]{_t} [= `Left] if [t >= User.left].}} *)
  (** {1 Touch target and finger sizes} 

      These values are from 
      {{:http://msdn.microsoft.com/en-us/library/windows/apps/hh465415.aspx#touch_targets}here}. 
*) 

  val touch_target_size : float 
  (** [touch_target_size] is [9.]mm, the recommended touch target size in 
      millimiters. *) 

  val touch_target_size_min : float 
  (** [touch_size_min] is [7.]mm, the minimal touch target size in 
      millimeters. *)

  val touch_target_pad : float 
  (** [touch_target_pad] is [2.]mm, the minimum padding size in 
      millimeters between two touch targets. *) 

  val average_finger_width : float 
  (** [average_finger_width] is [11.]mm, the average {e adult} finger width. *)

end

(** Rendering surface. 

    An application has a single rendering surface.*)
module Surface : sig

  (** {1 Properties} *)

  val size : size2 signal 
  (** [size] is the application's rendering surface size. 
      Differs from {!App.size} on high-dpi displays. *)

  (** {1 Refreshing} 

      {b TODO} Describe general idea: have the client do 
      the right thing w.r.t. to rendering/input gather and 
      on some systems drawing only refresh events is good 
      for power savings (e.g. we use request animation frame 
      in the JavaScript backend). *) 

  val update : unit -> unit 
  (** [update ()] updates the rendering surface. *) 

  val refresh : float event
  (** [refresh] occurs whenever the surface needs to be redrawn with
      the number of seconds since its last occurence or, for the
      first occurence, the number of seconds since the application
      start.

      The exact occurence frequency is unspecified but generally it
      should not exceed the {e hinted} frequency of {!refresh_hz}
      hertz.

      More precisely the following occurences are guaranteed:
      {ul 
      {- {e Simultaneously} with {!App.start}.}
      {- Some time {e after} the {!size} signal changes value.}
      {- Some time {e after} an occurence of the event set by 
         {!set_refresher}.}
      {- As a side effect of invocations to {!send_refresh}
         or {!animate}.}} *)

  val request_refresh : unit -> unit 
  (** TODO review that *) 

  val set_refresher : 'a event -> unit 
  (** [set_refresher r] uses the occurence of [r] to ask for
      {!refresh} occurences at a maximal hinted frequency of
      {!refresh_hz} hertz. Generated [refresh] occurences will not be
      simultaneous with [r]. *)

  val steady_refresh : until:'a event -> unit
  (** [steady_refresh until] has the side effect of making {!refresh}
      occur at a hinted frequency of {!refresh_hz} hertz at least
      until [until] occurs. If [steady_refresh] is called more than
      once, the property is maintainted for each provided
      [until]. *)

  val animate : span:float -> float signal
  (** [animate span] is a signal that increases from [0.] to [1.]
      during [span] seconds with the side effect of making
      [refresh] occur at a hinted frequency of {!refresh_hz} until 
      at least [span] is over. *)
      
  val refresh_hz : int signal 
  (** [refresh_hz] is the {e hinted} frequency in hertz for {!refresh}
      occurences initiated by calls to {!send_refresh} and
      {!animate}. The initial value is [60]. *)

  val set_refresh_hz : int -> unit
  (** [set_refresh_hz] sets the value of {!refresh_hz}. *)

  (** {1 Specification} *) 
    
  type colors = [ `RGBA_8888 | `RGB_565 ]
  (** The type for color buffers specification. *) 

  type depth = [ `D_24 | `D_16 ]
  (** The type for depth buffer specification. *) 

  type stencil = [ `S_8 ]
  (** The teype for stencil buffer specification. *)

  type spec
  (** The type for surface specification. *)

  val spec : 
    ?share:bool -> 
    ?accelerated:bool option ->
    ?multisample:int option ->
    ?doublebuffer:bool -> 
    ?stereo:bool ->
    ?srgb:bool ->
    ?colors:colors ->
    ?depth:depth option -> 
    ?stencil:stencil option ->
    gl:(int * int) -> unit -> spec
  (** 
    [spec share accelerated multisample doublebuffer
     stereo srgb colors depth stencil gl ()] is a surface specification value 
     such that:
    {ul 
    {- [share], share resources with current context. Defaults to [false].} 
    {- [accelerated], use [Some false] for software renderer, [Some true]
       to require hardware renderer, [None] to allow either. Defaults to 
       [None]}
    {- [multisample], use [Some count] for enabling a multisample buffer
       with [count] samples. Defaults to [Some 8].}
    {- [stereo], use stereo buffers. Defaults to [false].} 
    {- [srgb], request an sRGB framebuffer. Defaults to [true].} 
    {- [colors], specify the color buffers. Defaults to [`RGBA_8888].}
    {- [depth], specify the depth buffer. Defaults to [Some `D_24].}
    {- [sentcil], specify the stencil buffer. Defaults to [None].}
    {- [gl], specify the GL version.}} *)
end

(** Application  *) 
module App : sig
  
  val prefs_path : org:string -> app:string -> 
    [`Ok of string | `Error of string ]
  (** [TODO] this should used the app name automatically. 
      Unique to user and app. *) 

  (** {1 Properties} *) 

  val size : size2 signal
  (** [size] is the application's size. *)
      
  val pos : p2 signal 
  (** [pos] is the application's position. *) 

  val env : string -> default:'a -> (string -> 'a) -> 'a 
  (** [env var ~default parse] lookups [var] in the environment, parses
      it with [parse] and returns the result. If [parse] raises or 
      if [var] is not in the environment [default] is returned. *)

  (** {1 Mode} *)

  type mode = [ `Windowed | `Fullscreen ] 
  (** The type for application modes. *)

  val mode : mode signal 
  (** [mode] is the application's mode. This signal 
      is defined by the signal given to {!App.init}. *)

  val mode_switch : ?init:mode -> 'a event -> mode signal 
  (** [mode_switch init e] has value [init] (defaults to `Windowed) and 
      switches mode on each occurence of [e]. *) 

  (** {1 Init, run and release} *)

  val init :
    ?hidpi:bool -> 
    ?pos:p2 -> 
    ?size:size2 -> 
    ?name:string -> 
    ?surface:Surface.spec ->
    ?mode:mode signal -> 
    unit -> [ `Ok of unit | `Error of string ]
  (** [init pos size name gl_conf ()] is an app. 
      {ul 
      {- [hidpi] if [true] (default) tries to get a high-dpi surface.}
      {- [mode], defines the application mode and the {!value:mode} signal, 
         defaults to [S.const `Windowed].}} *)

  val run_step : unit -> Time.span 
  (** [run_step ()] gather as much user input as possible and returns 
      the maximal timespan after which {!run_step} should be called again. *)
  
  val run : ?until:'a event -> unit -> unit
  (** [run until] depends on the {{!backend_scheme}backend scheme}:
      {ul 
      {- [`Sync] invokes {!run_step} repeatedly and blocks until 
         the first occurence of [until] (defaults to {!quit}).
         After {!run} returned it can be called again.}
      {- [`Async] returns immediately, [until] is irrelevant.}} *)

  val release : ?sinks:bool -> unit -> unit 
  (** [release sinks ()] does the following:
      {ol 
      {- Makes the {!stop} event occur.}
      {- If [sinks] is [true] (default), calls {!release_sinks}.}
      {- Reclaims other resources}}
      
      After a {!release} it should be possible to {!init} again. *)

  val start : unit event
  (** [start] occurs the first time either {!run_step} or {!run} is 
      called. *)

  val stop : unit event 
  (** [stop] occurs when {!release} starts. *)

  (** {1 User requested quit} *)

  val quit : unit event
  (** [quit] occurs whenever the user requested to quit. The meaning
      depends on the {{!backend}backend}:
      {ul 
      {- [`Tsdl], this is only a hint, e.g. the last window 
         was closed or a platform dependent way of quitting 
         applications was invoked}
      {- [`Jsoo], the browser window is closing and it's your 
         last chance to peform something}}. *)

  (** {1 Event and signal sinks} *) 

  val sink_event : 'a event -> unit 
  (** [sink_event e] keeps a reference on [e] until the app {!exit}s. *)
   
  val sink_signal : 'a signal -> unit 
  (** [sink_signal s] keeps a reference on [s] until the app {!exit}s. *) 

  val release_sinks : unit -> unit
  (** Stops and release sink references. In the [`Jsoo] {{!backend}backend}
      stops are {{!React.strongstop}strong}. *)

  (** {1 Launch context} *) 

  type launch_context = [ `Browser | `Gui | `Terminal ]
    
  val launch_context : launch_context
  (** [launch_context] is the mechanism that started the program. *)

  val pp_launch_context : Format.formatter -> launch_context -> unit 
  (** [pp_launch_context ppf c] prints an unspecified representation of 
      [c] on [ppf]. *) 

  (** {1 Platform and backend} *) 

  val platform : string
  (** [platform] is the name of the platform you are running on. 

      {b Warning.} Do not expect different backend to report the same
      platform with the same string. *)

  type backend = [ `Tsdl | `Jsoo | `Other of string ] 
  (** The type for [Useri]'s backends. *) 

  val backend : backend 
  (** [backend] is [Useri]'s current backend. *)

  val pp_backend : Format.formatter -> backend -> unit
  (** [pp_backend ppf b] prints an unspecified representation of 
      [c] on [ppf]. *)

  type backend_scheme = [ `Sync | `Async ]
  (** The type for backend scheme.
      {ul 
      {- [`Sync], the backend is synchronous the client of the library 
         is in charge of running the event loop by using {!run_step} 
         or {!run}.}
      {- [`Async], the backend is asynchronous, there's an inversion 
         of control, the call to {!run} won't block.}} *)

  val backend_scheme : [ `Sync | `Async ]
  (** [backend_scheme] is the [Useri]'s current backend's scheme. *)

  val pp_backend_scheme : Format.formatter -> backend_scheme -> unit 
  (** [pp_backend_scheme ppf bs] prints and unspecified representation of
      [bs] on [ppf]. *)
                        
  (** {1 CPU count} *)
    
  type cpu_count = [ `Known of int | `Unknown ]
  (** The type for CPU counts. *) 

  val cpu_count : cpu_count 
  (** [cpu_count] is the number of CPU available. *)

  val pp_cpu_count : Format.formatter -> cpu_count -> unit 
  (** [pp_cpu_count ppf c] prints an unspecified representation of [c]
      on [ppf]. *)
end

(** 
   
   {1 Minimal example} 

   This minimal example can be used on both synchronous 
   and asynchronous backends, note however that in the latter
   {!App.release} is not called. 
{[
  
]}
   
   


   {1:cooperate Integration with cooperative concurency}

    A good way of managing side-effects at the boundaries of your
    functional reactive system is to use a cooperative concurency
    library and convert event occurences and signal changes to
    yielding futures/threads (to avoid the problem of forbidden
    recursive primitive feedback) and convert futures/threads to a
    primitive event with a single occurence.

    You will need however need to cooperate with [Useri]'s event 
    loop and give it a high priority as the ability to interact 
    should {b never} take over a long running computation.

    The following code shows how to do that with Lwt and Fut
{[
]}
*) 



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
