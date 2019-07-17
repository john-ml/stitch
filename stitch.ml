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
  let open Name in
  let open Node in
  let open Ty in
  let open Typing in
  let empty = 
    { Ctx
    . locals = NameM.empty
    ; globals = NameM.empty
    ; aliases = NameM.empty
    ; uf = Uf.empty
    ; insts = MetaM.empty
    ; rec_insts = MetaM.empty
    ; apps = AppI.empty
    }
  in
  let p, _q, _r, _s = Meta.(fresh (), fresh (), fresh (), fresh ()) in
  let x, y, z, w = Meta.(fresh (), fresh (), fresh (), fresh ()) in
  let dump k =
    try print_endline (Ctx.show (k empty))
    with Mismatch (c, want, have, mmsg) ->
      print_endline (Printf.sprintf
        "Expected %s but got %s%s. Context: %s"
        (Ty.show want) (Ty.show have)
        (match mmsg with None -> "" | Some msg -> " ("^msg^")")
        (Ctx.show c))
  in
  let f t = at (Ptr (Meta p, t)) in
  let mx, my, mz, mw = at (Meta x), at (Meta y), at (Meta z), at (Meta w) in
  (* μ x. *x ~ μ y. *y *)
  dump (fun c -> c
    |> unify mx (f mx)
    |> unify my (f my)
    |> unify mx my);
  (* μ x. *x ~ μ y. **y *)
  dump (fun c -> c
    |> unify mx (f mx)
    |> unify my (f (f my))
    |> unify mx my);
  (* μ x. f^10(x) ~ μ y. f^11(y) *)
  dump (fun c -> c
    |> unify mx (f (f (f (f (f (f (f (f (f (f mx))))))))))
    |> unify my (f (f (f (f (f (f (f (f (f (f (f my)))))))))))
    |> unify mx my);
  (* ?x = f(?y), ?y = f(?x), ?z = f(?z) ==> ?x ~ ?z *)
  dump (fun c -> c
    |> unify mx (f my)
    |> unify my (f mz)
    |> unify mz (f mz)
    |> unify mx mz);
  (* ?x = f(?y), ?y = f(?y) ==> ?x ~ ?y *)
  dump (fun c -> c
    |> unify mx (f my)
    |> unify my (f my)
    |> unify mx my);
  (* ?x = f(f(?x)) ==> ?x ~ f(?x) *)
  dump (fun c -> c
    |> unify mx (f (f mx))
    |> unify mx (f mx));
  (* x /~ y *)
  dump (fun c -> c |> unify (at (Var (id "x"))) (at (Var (id "y"))));
  dump (fun c -> c
    |> unify mx my
    |> unify mz mw
    |> unify mx mw);
  (* μ x. f^10(x) ~ μ y. f^11(z) *)
  dump (fun c -> c
    |> unify mx (f (f (f (f (f (f (f (f (f (f mx))))))))))
    |> unify my (f (f (f (f (f (f (f (f (f (f (f (at (Var (id "z"))))))))))))))
    |> unify mx my);
  let int = at (Lit (id "int")) in
  let idx, idy = id "x", id "y" in
  (* {x int} /~ {y int} *)
  dump (unify 
    (at (Rec (Cons (NameM.singleton idx int, Nil))))
    (at (Rec (Cons (NameM.singleton idy int, Nil)))));
  (* {x int} ~ {x int} *)
  dump (unify 
    (at (Rec (Cons (NameM.singleton idx int, Nil))))
    (at (Rec (Cons (NameM.singleton idx int, Nil)))));
  (* {x int; ?z} ~ {x int; ?w} *)
  dump (unify 
    (at (Rec (Cons (NameM.singleton idx int, Open z))))
    (at (Rec (Cons (NameM.singleton idx int, Open w)))));
  (* {x int; ?z} ~ {y int; ?w} *)
  dump (unify 
    (at (Rec (Cons (NameM.singleton idx int, Open z))))
    (at (Rec (Cons (NameM.singleton idy int, Open w)))))
