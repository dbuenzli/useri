open Ocamlbuild_plugin

let () = 
  dispatch begin fun d -> 
    Ocamlbuild_js_of_ocaml.dispatcher d; 
    match d with
    | After_rules -> 
        flag ["js_of_ocaml"; "weak_js"] (A "+weak.js");
        ocaml_lib "src/tsdl/useri_tsdl"; 
        ocaml_lib "src/jsoo/useri_jsoo";
        ocaml_lib "src/useri"
    | _ -> () 
  end
