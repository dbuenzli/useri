* Try the Remove the src/$B/src_backend.mli links. I think the culprit is 
  ocamlbuild whose build are not isolated enough. Try with assemblage.
* Time.span rather use ms ? 
* Add locale lookup
* Add open_uri
* Surface, make it independent from Gl. I.e. we may what to use 
  that for Canvas aswell.
* Minimal examples.
* Multi-surfaces ? An approach would be to keep the events as 
  they are but have a notion of active surface.
* Tsdl, fix any_repeat, stops after multikey.
* Tsdl, fix set_clipboard, doesn't seem to survive program exit.
* Support for touch.
* Jsoo: handle directories, see 
  http://updates.html5rocks.com/2012/07/Drag-and-drop-a-folder-onto-Chrome-now-available
  match Tsdl backend (to verify, generate one Drop.file event per directory
  entry).
