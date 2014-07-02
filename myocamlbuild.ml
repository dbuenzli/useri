open Ocamlbuild_plugin

let () = 
  dispatch begin fun d -> 
    Ocamlbuild_js_of_ocaml.dispatcher d; 
    match d with
    | After_rules -> 
        ocaml_lib "src/tsdl/useri_tsdl"; 
        ocaml_lib "src/jsoo/useri_jsoo";
    | _ -> () 
  end
