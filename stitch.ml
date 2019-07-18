open Misc
open Syntax

let _ =
  let open Ty in let open Notation in
  let open Expr in let open Notation in
  let old_margin = Format.pp_get_margin Format.std_formatter () in
  Format.pp_set_margin Format.std_formatter 30;
  let dump e = Format.printf "%a@." Expr.pp e in
  dump (~$"f" *$ ["Some"*^[]*."field"]);
  let nested =
    ~$"y" += ~& (~$"x" *! ~!3) *:: ~*(~$$"i32") @@
    ~!0
  in
  dump (~$"f"*$["Some"*^[nested]*."field"]);
  dump (ite None ~$"x" ~!0 ~!1);
  dump (ite None ~$"x" ~!0 nested);
  dump (ite None ~$"x" nested ~!1);
  dump (ite None ~$"x" nested nested);
  dump (ite None nested ~!0 ~!1);
  dump (ite None nested ~!0 nested);
  dump (ite None nested nested ~!1);
  dump (ite None nested nested nested);
  Format.pp_set_margin Format.std_formatter old_margin

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

let empty_ctx = 
  let open Typing in
  { Ctx
  . locals = NameM.empty
  ; globals = NameM.empty
  ; aliases = NameM.empty
  ; uf = Uf.empty
  ; insts = MetaM.empty
  ; rec_insts = MetaM.empty
  ; apps = AppI.empty
  ; trapped = MetaM.empty
  ; poly_insts = MetaM.empty
  ; constraints = NameM.empty
  }

let dump_ctx ~succeeds k =
  let open Typing in
  let failed () = if succeeds then failwith "Shouldn't have failed" else () in
  begin try
    print_endline (Ctx.show (k empty_ctx));
    if not succeeds then failwith "Shouldn't have succeeded" else ()
  with
  | Ctx.Mismatch (c, want, have, mmsg) ->
    print_endline (Printf.sprintf
      "Expected %s, got %s%s. Context: %s"
      (Ty.show want) (Ty.show have)
      (match mmsg with None -> "" | Some msg -> " ("^msg^")")
      (Ctx.show c));
    failed ()
  | Ctx.RowDup (c, x, t) ->
    print_endline (Printf.sprintf
      "Field %s is duplicated in %s. Context: %s"
      (Name.show x) (Ty.show t) (Ctx.show c));
    failed ()
  end;
  print_endline ""

let _ =
  let open Name in
  let open Node in
  let open Ty in
  let open Typing in
  let p, _q, r, _s = Meta.(fresh (), fresh (), fresh (), fresh ()) in
  let x, y, z, w = Meta.(fresh (), fresh (), fresh (), fresh ()) in
  let f t = at (Ptr (Meta p, t)) in
  let mx, my, mz, mw = at (Meta x), at (Meta y), at (Meta z), at (Meta w) in
  (* μ x. *x ~ μ y. *y *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx (f mx)
    |> unify my (f my)
    |> unify mx my);
  (* μ x. *x ~ μ y. **y *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx (f mx)
    |> unify my (f (f my))
    |> unify mx my);
  (* μ x. f^10(x) ~ μ y. f^11(y) *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx (f (f (f (f (f (f (f (f (f (f mx))))))))))
    |> unify my (f (f (f (f (f (f (f (f (f (f (f my)))))))))))
    |> unify mx my);
  (* ?x = f(?y), ?y = f(?x), ?z = f(?z) ==> ?x ~ ?z *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx (f my)
    |> unify my (f mz)
    |> unify mz (f mz)
    |> unify mx mz);
  (* ?x = f(?y), ?y = f(?y) ==> ?x ~ ?y *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx (f my)
    |> unify my (f my)
    |> unify mx my);
  (* ?x = f(f(?x)) ==> ?x ~ f(?x) *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx (f (f mx))
    |> unify mx (f mx));
  (* x /~ y *)
  dump_ctx ~succeeds:false (fun c -> c
    |> unify (at (Var (id "x"))) (at (Var (id "y"))));
  (* ?x ~ ?y, ?z ~ ?w, ?x ~ ?w *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify mx my
    |> unify mz mw
    |> unify mx mw);
  (* μ x. f^10(x) /~ μ y. f^11(z) *)
  dump_ctx ~succeeds:false (fun c -> c
    |> unify mx (f (f (f (f (f (f (f (f (f (f mx))))))))))
    |> unify my (f (f (f (f (f (f (f (f (f (f (f (at (Var (id "z"))))))))))))))
    |> unify mx my);
  let int = at (Lit (id "int")) in
  let idx, idy, idz = id "x", id "y", id "z" in
  (* {x int} /~ {y int} *)
  dump_ctx ~succeeds:false (unify
    (at (Rec (Cons (NameM.singleton idx int, Nil))))
    (at (Rec (Cons (NameM.singleton idy int, Nil)))));
  (* {x int} ~ {x int} *)
  dump_ctx ~succeeds:true (unify 
    (at (Rec (Cons (NameM.singleton idx int, Nil))))
    (at (Rec (Cons (NameM.singleton idx int, Nil)))));
  (* {x int; ?z} ~ {x int; ?w} *)
  dump_ctx ~succeeds:true (unify 
    (at (Rec (Cons (NameM.singleton idx int, Open z))))
    (at (Rec (Cons (NameM.singleton idx int, Open w)))));
  (* {x int; ?z} ~ {y int; ?w} *)
  dump_ctx ~succeeds:true (unify 
    (at (Rec (Cons (NameM.singleton idx int, Open z))))
    (at (Rec (Cons (NameM.singleton idy int, Open w)))));
  (* {x int; ?r} ~ {y int; ?w} ==> {z int; ?r} ~ {y int, z int} *)
  dump_ctx ~succeeds:true (fun c -> c
    |> unify 
         (at (Rec (Cons (NameM.singleton idx int, Open r))))
         (at (Rec (Cons (NameM.singleton idy int, Open w))))
    |> unify
         (at (Rec (Cons (NameM.singleton idz int, Open r))))
         (at (Rec (Cons (NameM.of_list [idy, int; idz, int], Nil)))));
  (* {x int; ?r} ~ {y int; ?w} ==> {z int; ?r} /~ {y int, z int; ?w} *)
  dump_ctx ~succeeds:false (fun c -> c
    |> unify 
         (at (Rec (Cons (NameM.singleton idx int, Open r))))
         (at (Rec (Cons (NameM.singleton idy int, Open w))))
    |> unify
         (at (Rec (Cons (NameM.singleton idz int, Open r))))
         (at (Rec (Cons (NameM.of_list [idy, int; idz, int], Open w)))));
  (* {; ?r} ~ {x int} ==> {x int; ?r} ~ ?y *)
  dump_ctx ~succeeds:false (fun c -> c
    |> unify 
         (at (Rec (Cons (NameM.empty, Open r))))
         (at (Rec (Cons (NameM.singleton idx int, Nil))))
    |> unify
         (at (Rec (Cons (NameM.singleton idx int, Open r))))
         (at (Meta y)))

let _ =
  let open Typing in
  let open Ty in let open Notation in
  let open Expr in let open Notation in
  let dump_ctx_infer ~succeeds e =
    dump_ctx ~succeeds (fun c ->
      let c = {c with globals = Expr.env; aliases = Ty.env} in
      let c, t = infer_expr e c in
      print_endline (Ty.show t);
      c)
  in
  dump_ctx_infer ~succeeds:true
    ("x" *: !?() -= ~!3 @@
     "y" *: !?() -= ~%0.1 @@
     ~$"x");
  dump_ctx_infer ~succeeds:false (~%0.1 *:: ~$$"i64");
  dump_ctx_infer ~succeeds:false (~!1 *:: ~$$"f64");
  dump_ctx_infer ~succeeds:true (~!1 *:: ~$$"i64");
  dump_ctx_infer ~succeeds:true ("__add__"*@@[~!1; ~!2]);
  dump_ctx_infer ~succeeds:false ("__add__"*@@[~!1; ~%2.0]);
  dump_ctx_infer ~succeeds:false ("__add__"*@@[~!1; ~!2; ~!3]);
  dump_ctx_infer ~succeeds:true (ite None ~!0 ~!1 ("__add__"*@@[~!2; ~!3]));
  dump_ctx_infer ~succeeds:false (ite None ~!0 ~!1 ("__add__"*@@[~%2.0; ~%3.0]))
