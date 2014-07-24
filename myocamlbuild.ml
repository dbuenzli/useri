open Ocamlbuild_plugin

let () =
  dispatch begin fun d ->
    Ocamlbuild_js_of_ocaml.dispatcher d;
    match d with
    | After_rules ->
        flag ["js_of_ocaml"; "weak_js"] (A "+weak.js");
        ocaml_lib ~dir:"src/tsdl" "src/tsdl/useri_tsdl";
        ocaml_lib ~dir:"src/jsoo" "src/jsoo/useri_jsoo";
        ocaml_lib ~dir:"src" "src/useri_base";
        ocaml_lib ~dir:"src" "src/useri"
    | _ -> ()
  end
