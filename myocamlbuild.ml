open Ocamlbuild_plugin

let () =
  dispatch begin fun d ->
    Ocamlbuild_js_of_ocaml.dispatcher d;
    match d with
    | After_rules ->
        (* FIXME: hack to get the correct link order. *)
        flag ["basefirst"; "byte"; "link"] (A "src/useri_base.cma");
        flag ["basefirst"; "native"; "link"] (A "src/useri_base.cmxa");
        flag ["js_of_ocaml"; "weak_js"] (A "+weak.js");
        ocaml_lib ~dir:"src/tsdl" "src/tsdl/useri_tsdl";
        ocaml_lib ~dir:"src/jsoo" "src/jsoo/useri_jsoo";
        ocaml_lib ~dir:"src" "src/useri_base";
        ocaml_lib ~dir:"src" "src/useri"
    | _ -> ()
  end
