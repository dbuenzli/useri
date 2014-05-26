#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let tsdl = Env.bool "tsdl"
let jsoo = Env.bool "jsoo"

let () = 
  Pkg.describe "useri" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "src/useri_top";
    Pkg.lib ~exts:Exts.interface "src/useri";
    Pkg.lib ~cond:tsdl ~exts:Exts.library "src/tsdl/useri_tsdl";
    Pkg.lib ~cond:jsoo ~exts:Exts.library "src/jsoo/useri_jsoo";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
