open Misc
open Syntax

let _ =
  let open Node in
  let open Name in
  let open Ty in
  let open Meta in
  let open Expr in
  print_endline "----- Without nesting:";
  print_endline (Doc.render (doc (
    mk_app (mk_var "f") [mk_proj (mk_inj "Some" []) "field"])));
  print_endline "----- With nesting:";
  let nested =
    mk_set (mk_var "y")
      (mk_ann
        (mk_ref (mk_ind (mk_var "x") (mk_int 3)))
        (at (Ptr (Meta (fresh ()), at (Lit (id "i32"))))))
      (mk_int 0)
  in
  print_endline (Doc.render (doc (
    mk_app (mk_var "f") [mk_proj (mk_inj "Some" [nested]) "field"])));
  if false then begin
    print_endline (Doc.render (doc (
      ite None (mk_var "x") (mk_int 0) (mk_int 1))));
    print_endline (Doc.render (doc (
      ite None (mk_var "x") (mk_int 0) nested)));
    print_endline (Doc.render (doc (
      ite None (mk_var "x") nested (mk_int 1))));
    print_endline (Doc.render (doc (
      ite None (mk_var "x") nested nested)));
    print_endline (Doc.render (doc (
      ite None nested (mk_int 0) (mk_int 1))));
    print_endline (Doc.render (doc (
      ite None nested (mk_int 0) nested)));
    print_endline (Doc.render (doc (
      ite None nested nested (mk_int 1))));
    print_endline (Doc.render (doc (
      ite None nested nested nested)))
  end else
    ()

let _ =
  let open Uf in
  let x, y, z, w = Meta.fresh (), Meta.fresh (), Meta.fresh (), Meta.fresh () in
  Printf.printf "x = %d, y = %d, z = %d, w = %d\n" x y z w;
  let dump uf =
    Printf.printf "find(x) = %d, find(y) = %d, find(z) = %d, find(w) = %d\n" 
      (find x uf) (find y uf) (find z uf) (find w uf)
  in
  let uf = empty in
  dump uf;
  let uf = union x y uf in
  dump uf;
  let uf = union z w uf in
  dump uf;
  let uf = union w y uf in
  dump uf

let _ =
  let open Typing in
  infer
