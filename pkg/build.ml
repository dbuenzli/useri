#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let tsdl = Env.bool "tsdl"
let jsoo = Env.bool "jsoo"

let builder = 
  if not jsoo then `OCamlbuild else 
  `Other
    ("ocamlbuild -use-ocamlfind -classic-display \
                 -plugin-tag \"package(js_of_ocaml.ocamlbuild)\"", 
     "_build")

let () = 
  Pkg.describe "useri" ~builder:`Other(builder,"_build") [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "src/useri_top";
    Pkg.lib ~exts:Exts.interface "src/useri";
    Pkg.lib ~cond:tsdl ~exts:Exts.library "src/tsdl/useri_tsdl";
    Pkg.lib ~cond:jsoo ~exts:Exts.library "src/jsoo/useri_jsoo";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
