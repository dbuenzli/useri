#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let tsdl = Conf.with_pkg "tsdl"
let jsoo = Conf.with_pkg "js_of_ocaml"

let jsoo_test ~cond test =
  Pkg.flatten
    [ Pkg.test ~run:false ~cond ~auto:false (test ^ ".js");
      Pkg.test ~run:false ~cond ~auto:false (test ^ ".html"); ]

let () =
  Pkg.describe "useri" @@ fun c ->
  let tsdl = Conf.value c tsdl in
  let jsoo = Conf.value c jsoo in
  Ok [ Pkg.mllib "src/useri.mllib";
       Pkg.mllib ~api:[] "src/useri_top.mllib";
       Pkg.mllib "src/useri_base.mllib";
       Pkg.lib ~exts:Exts.interface "src/useri_backend";
       Pkg.mllib ~cond:tsdl ~api:["Useri_tsdl"]
         "src-tsdl/useri_tsdl.mllib" ~dst_dir:"tsdl";
       Pkg.mllib ~cond:jsoo ~api:["Useri_jsoo"]
         "src-jsoo/useri_jsoo.mllib" ~dst_dir:"jsoo";
(*     Pkg.test ~run:false "test/test_tsdl";
       Pkg.test ~run:false ~cond:tsdl "test/test_tsdl_fut";
       Pkg.test ~run:false ~cond:tsdl "test/test_tsdl_lwt";
       Pkg.test ~run:false ~cond:tsdl "test/min";
       jsoo_test ~cond:jsoo "test/min"; *)
       jsoo_test ~cond:jsoo "test/test_jsoo";
       jsoo_test ~cond:jsoo "test/test_jsoo_fluid";
       jsoo_test ~cond:jsoo "test/chain"
 ]
