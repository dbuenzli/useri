open Ocamlbuild_plugin

let () = 
  dispatch begin function 
  | After_rules -> 
      ocaml_lib "src/tsdl/useri_tsdl"; 
      ocaml_lib "src/jsoo/useri_jsoo";
  | _ -> () 
  end
