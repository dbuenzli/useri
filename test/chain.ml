(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Chain following the mouse.
   Adapted from: http://processing.org/examples/follow3.html
   But composable... *)

open Gg
open Vg
open React
open Useri

let log fmt = Format.printf (fmt ^^ "@.")

module Chain : sig
  type t
  val create : p2 -> t
  val move_to : p2 -> t -> t
  val to_image : t -> Vg.image
end = struct
  type t = p2 list

  let link_len = 0.04

  let create loc =
    let rand () = Float.random ~min:(-. link_len) ~len:(2. *. link_len) () in
    let rand_dv () = V2.v (rand ()) (rand ()) in
    let rec pts n acc =
      if n = 0 then acc else
      pts (n - 1) (V2.(loc + rand_dv ()) :: acc)
    in
    pts 24 []

  let move_to new_pt chain =
    let rec move acc new_pt = function
    | [] -> List.rev acc
    | pt :: pts ->
        let theta = V2.(angle (new_pt - pt)) in
        let pt' = V2.(new_pt - (polar link_len theta)) in
        move (pt' :: acc) pt' pts
    in
    move [new_pt] new_pt (List.tl chain)

  let to_image chain =
    let color = I.const (Color.gray ~a:0.32 1.) in
    let area = `O { P.o with P.width = 0.02; cap = `Round } in
    let rec segs acc start = function
    | [] -> acc
    | pt :: pts ->
        let p = P.(empty >> sub start >> line pt) in
        let acc' = I.(acc >> blend @@ cut ~area p color) in
        segs acc' pt pts
    in
    segs I.void (List.hd chain) (List.tl chain)
end

let chain aspect =
  let scale = S.map (fun a -> V2.v a 1.) aspect in
  let locs = S.changes (S.l2 V2.mul scale Mouse.pos) in
  let init = Chain.create V2.(mul (S.value scale) (v 0.5 0.5)) in
  S.accum (E.map Chain.move_to locs) init

let render_scene r _ (aspect, chain) =
  let view = Box2.v P2.o (Size2.v aspect 1.) in
  let renderable = `Image (Size2.unit, view, Chain.to_image chain) in
  ignore (Vgr.render r renderable);
  ()

let setup () =
  let aspect = S.map Size2.aspect Surface.size in
  let scene = S.Pair.pair aspect (chain aspect) in
  let c = Useri_jsoo.Surface.Handle.to_js (Surface.handle ()) in
  let r = Vgr.create (Vgr_htmlc.target ~resize:false c) `Other in
  App.sink_event (S.sample (render_scene r) Surface.refresh scene);
  Surface.set_refresher (S.changes Mouse.pos);
  Surface.(set_mode_setter @@ mode_flip (Key.up `Space));
  ()

let main () =
  let surface = Surface.create ~kind:`Other () in
  match App.init ~surface () with
  | `Error e -> Printf.eprintf "%s" e; exit 1
  | `Ok () -> setup (); App.run ()

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
