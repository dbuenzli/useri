* Review all M.release.
* Try the Remove the src/$B/src_backend.mli links. I think the culprit is
  ocamlbuild whose build are not isolated enough. Try with assemblage.
* TODO desync refresh from anim updates.
* Add locale lookup
* Add open_uri
* Minimal examples.
* Rename anchor_of_canvas to of_js
* Multi-surfaces ? An approach would be to keep the events as
  they are but have a notion of active surface.
* Tsdl, add anchor
* Support for touch.
* Fullscreen, in jsoo the app may define mode, better have
  an event.
* Mouse.{left,middle,right}_double.
* Jsoo: handle more of the Text api. This may need a rework of Key aswell,
  we could try to always maintain an input element at the same location
  as the canvas, click on canvas/focus on canvas should focus the
  the input element which should handle both key and text input. Rather
  than canvas handling it right now.
* Jsoo: handle directories, see
  http://updates.html5rocks.com/2012/07/Drag-and-drop-a-folder-onto-Chrome-now-available
  match Tsdl backend (to verify, generate one Drop.file event per directory
  entry).
