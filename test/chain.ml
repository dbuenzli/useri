(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Chain following the mouse.
   Adapted from: http://processing.org/examples/follow3.html
   But composable... *)

open Gg
open Vg
open React
open Useri
open Result

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
  | Error (`Msg m) -> Printf.eprintf "%s" m; exit 1
  | Ok () -> setup (); App.run ()

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
