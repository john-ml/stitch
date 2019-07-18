open Misc
open Node
open Syntax

(* Each type application f(?X, ..) is associated with a unique meta *)
module AppI = Iso (Meta) (OrdPair (Name) (OrdList (Meta)))

(* Type-checking context *)
module Ctx = struct

type t =
  { (* Values / monomorphic fns *) locals: Ty.t NameM.t      
  ; (* Global & polymorphic fns *) globals: Ty.poly NameM.t  
  ; (* Type aliases             *) aliases: Ty.alias NameM.t 
  ; (* Equalities among metas   *) uf: Uf.t                  
  ; (* Instantiations for metas *) insts: Ty.t MetaM.t       
  ; (* Coinductive insts        *) rec_insts: TyS.t MetaM.t
  ; (* Metas for applications   *) apps: AppI.t              
  ; (* Metas that can't escape  *) trapped: Span.t MetaM.t
  ; (* Insts for poly fn apps   *) poly_insts: MetaS.t MetaM.t
  ; (* Trait requirements       *) constraints: MetaS.t NameM.t
  }

exception Unbound of Span.t * Name.t
exception Mismatch of t * Ty.t * Ty.t * string option
exception RowDup of t * Name.t * Ty.t
exception Escaping of Span.t * Name.t

let require (x: Meta.t) (trait: Name.t) (c: t): t =
  {c with constraints =
     NameM.update trait
      (function
       | Some xs -> Some (MetaS.add x xs)
       | None -> Some (MetaS.singleton x))
      c.constraints}

let extend (x: Name.t) (t: Ty.t) (c: t): t =
  {c with locals = NameM.add x t c.locals}
    
let lookup sp (x: Name.t) (c: t): Ty.t =
  try NameM.find x c.locals
  with Not_found -> raise (Unbound (sp, x))

let lookup_global sp (x: Name.t) (c: t): Ty.poly =
  try NameM.find x c.globals
  with Not_found -> raise (Unbound (sp, x))

(* Uf.find(') lifted to contexts *)
let find' x c = let uf, x = Uf.find' x c.uf in ({c with uf = uf}, x)

(* Uf.union lifted to contexts *)
let union x y c = {c with uf = Uf.union x y c.uf}

(* MetaM.add lifted to contexts *)
let add_inst x t c = {c with insts = MetaM.add x t c.insts}
let add_rec_inst x t c =
  {c with rec_insts =
     MetaM.update x
       (function Some s -> Some (TyS.add t s) | None -> Some (TyS.singleton t))
       c.rec_insts}
let add_poly_inst x (xs: MetaS.t) c =
  {c with poly_insts = MetaM.add x xs c.poly_insts}

(* AppI.add lifted to contexts *)
let add_app x fxs c = {c with apps = AppI.add x fxs c.apps}

let trap sp x c = {c with trapped = MetaM.add x sp c.trapped}

let occurs (x: Meta.t) (t: Ty.t) (c: t): bool =
  let open Ty in
  let visited = ref MetaS.empty in
  let x = Uf.find x c.uf in
  let rec go_row go go_elt = function
   | Nil | Closed _ -> false
   | Open x -> go (at (Meta x))
   | Cons (xts, r) -> NameM.exists (fun _ -> go_elt) xts || go_row go go_elt r
  in
  let rec go (t: Ty.t) =
    match t.a with
    | Lit _ | Var _ -> false
    | Ptr (r, t) | Lbl (r, t) -> go (at r) || go t
    | Rec r -> go_row go go r
    | Sum r -> go_row go (List.exists go) r
    | Fun (ts, r) -> List.exists go ts || go r
    | App (_, ts) -> List.exists go ts
    | Meta y ->
        let y = Uf.find y c.uf in
        if Meta.equal x y then true else
        if MetaS.mem y !visited then false else begin
          visited := MetaS.add y !visited;
          try go (MetaM.find y c.insts)
          with Not_found -> false
        end
  in
  go t

(* Check if any metas in `escaping` appear in any local variables *)
let check_escaping (escaping: Span.t MetaM.t) (c: t): unit =
  NameM.iter
    (fun y t ->
       MetaM.iter
         (fun x sp -> if occurs x t c then raise (Escaping (sp, y)))
         escaping)
    c.locals

(* Contexts are passed statefully through all typechecking functions, but:
   - .locals should behave more like Reader
   - When entering a new scope, .trapped should be cleared
   - When leaving a scope, the old .trapped should be restored and any metas
     accumulated in .trapped should be checked
   This helper wraps a stateful computation `f` with these behaviors. *)
let locally (c: t) (f: t -> t * 'a): t * 'a =
  let c', x = f {c with trapped = MetaM.empty} in
  let c = {c' with trapped = c.trapped; locals = c.locals} in
  check_escaping c'.trapped c;
  (c, x)

let show c =
  let open Show in
  let show_names = show_set NameS.elements Name.show in
  let show_namem show_v = show_map NameM.bindings Name.show show_v in
  let show_metam show_v = show_map MetaM.bindings Meta.show show_v in
  let show_locals = show_namem Ty.show in
  let show_globals = show_namem (show_pair (show_namem show_names) Ty.show) in
  let show_aliases = show_namem (show_pair (show_list Name.show) Ty.show) in
  let show_insts = show_metam Ty.show in
  let show_rec_insts = show_metam (show_set TyS.elements Ty.show) in
  let show_apps =
    show_iso
      AppI.equalities 
      Meta.show 
      (show_pair Name.show (show_list Meta.show))
  in
  let show_trapped = show_map MetaM.bindings Meta.show Span.show in
  "{locals = " ^ show_locals c.locals ^
  "; globals = " ^ show_globals c.globals ^
  "; aliases = " ^ show_aliases c.aliases^
  "; uf = " ^ Uf.show c.uf ^
  "; insts = " ^ show_insts c.insts ^
  "; rec_insts = " ^ show_rec_insts c.rec_insts ^
  "; apps = " ^ show_apps c.apps ^
  "; trapped = " ^ show_trapped c.trapped ^
  "}"

end

open Ctx

(* Helpers for working with rows/cols *)
let extract_row = function Ty.Rec r -> Some r | _ -> None
let extract_col = function Ty.Sum r -> Some r | _ -> None
let build_row r = Ty.Rec r
let build_col r = Ty.Sum r

(* Helper for `eapply` *)
let rec eapply_row go m r: Meta.t NameM.t * 'a Ty.row =
  let open Ty in
  match r with
  | Nil | Closed _ | Open _ -> (m, r)
  | Cons (xts, r) ->
      let m, xts =
        let open NameM in
        fold 
          (fun k v (m, xts) -> 
             let m, v = go m v in
             (m, add k v xts))
          xts (m, empty)
      in
      let m, r = eapply_row go m r in
      (m, Cons (xts, r))

(* Replace variables with metas as given by the mapping `m`.
   Replace variables not in `m` with fresh metas.
   Doesn't zonk anything.
   Return updated Name -> Meta mapping. *)
let eapply ?m:(m=NameM.empty): Ty.t -> Meta.t NameM.t * Ty.t =
  let open Ty in
  let rec go m t = 
    let m, ta =
      match t.a with
      | Lit _ | Meta _ -> (m, t.a)
      | Var x when NameM.mem x m -> (m, Meta (NameM.find x m))
      | Var x -> let y = Meta.fresh () in (NameM.add x y m, Meta y)
      | Ptr (x, t) -> let m, t = go m t in (m, Ptr (x, t))
      | Lbl (x, t) -> let m, t = go m t in (m, Lbl (x, t))
      | Rec r -> let m, r = eapply_row go m r in (m, Rec r)
      | Sum r -> let m, r = eapply_row go_list m r in (m, Sum r)
      | Fun (ts, r) ->
          let m, ts = go_list m ts in
          let m, r = go m r in
          (m, Fun (ts, r))
      | App (f, ts) -> let m, ts = go_list m ts in (m, App (f, ts))
    in
    (m, {t with a = ta})
  and go_list m ts =
    match ts with
    | [] -> (m, ts)
    | t :: ts ->
        let m, t = go m t in
        let m, ts = go_list m ts in
        (m, t :: ts)
  in
  go m

(* Helper for `unfold` *)
let unfold_row extract build: 'a Ty.row -> Ctx.t -> Ctx.t * 'a Ty.row =
  let rec go r c =
    let open Ty in
    match r with
    | Nil | Closed _ -> (c, r)
    | Open x ->
        (* Unfold row-metas *)
        let c, x = find' x c in
        (try let t = MetaM.find x c.insts in
             match extract t.a with
             | Some r ->
                 let c, r = go r c in
                 (* 'Path compression' on row-metas *)
                 (add_inst x (at ~sp:t.span (build r)) c, r)
             | _ -> assert false
         with Not_found -> (c, Open x))
    | Cons (xts, r) as row ->
        (* Recursively unfold nested rows *)
        let c, r = go r c in
        let xtsr =
          (* Collapse Cons (xts, Cons (yts, r)) -> Cons (xts ∪ yts, r) *)
          match r with
          | Cons (yts, r) ->
              let join x mss mts =
                match mss, mts with
                | None, None -> None
                | Some ts, None | None, Some ts -> Some ts
                | Some _, Some _ -> raise (RowDup (c, x, at (build row)))
              in
              Cons (NameM.merge join xts yts, r)
          | _ -> Cons (xts, r)
        in
        (c, xtsr)
  in
  go

(* Simplify a type so `unify` can understand it.

   let root x := x = Uf.find x c.uf in
   let stuck x := root x /\ ~ MetaM.mem x c.insts in
   forall let (c, r) := unfold c ty,
     let fix stuck_row r :=
       match r with
       | Nil | Closed _ -> True
       | Open x -> stuck x
       | Cons (_, Cons _) -> False
       | Cons (_, r) -> stuck_row r
       end
     in
     match r with
     | App _ -> False
     | Rec r | Sum r -> stuck_row r
     | _ -> True
     end
*)
let rec unfold (ty: Ty.t) (c: Ctx.t): Ctx.t * Ty.t =
  let open Ty in
  let wrap ta = at ~sp:ty.span ta in
  match ty.a with
  (* Root metas *)
  | Meta x -> let c, x = find' x c in (c, wrap (Meta x))
  (* Unfold row/col variables *)
  | Rec r -> let c, r = unfold_row extract_row build_row r c in (c, wrap (Rec r))
  | Sum r -> let c, r = unfold_row extract_col build_col r c in (c, wrap (Sum r))
  (* Turn alias applications into Metas *)
  | App (f, ts) ->
      let rec extract_metas = function
        | [] -> Some []
        | {a = Meta x; _} :: ts ->
            Option.map (fun ts -> x :: ts) (extract_metas ts)
        | _ :: _ -> None
      in
      (match
        Option.bind
          (fun ts -> AppI.findB_opt (f, ts) c.apps)
          (extract_metas ts)
       with
       (* If all ts are metas and App (f, ts) has already been assigned to a 
          meta, just return it *)
       | Some x -> (c, wrap (Meta x))
       (* Otherwise... *)
       | None ->
           (* eapply f's definition + associate each argument with a fresh meta x *)
           assert (NameM.mem f c.aliases);
           let args, ty = NameM.find f c.aliases in
           assert List.(length args = length ts);
           let arg_xs, ty = eapply ty in
           (* Associate each fresh meta with its corresponding argument t *)
           let c, xs =
             let open List in
             fold_left
               (fun (c, xs) (arg, t) ->
                  let x = NameM.find arg arg_xs in
                  (unify (at ~sp:t.span (Meta x)) t c, x :: xs))
               (c, [])
               (combine args ts)
           in
           (* Associate App (f, xs) with a fresh meta y,
              associate y with its unfolding, 
              and return Meta y 
           *)
           let y = Meta.fresh () in
           (c |> add_app y (f, xs) |> add_inst y ty, wrap (Meta y)))
  | _ -> c, ty

(* Unfold metas to expose a non-Meta constructor at root, if possible,
   and check if any coinductive hypotheses hold.

   forall updated wants and haves as r,
     match r with
     | Meta x -> stuck x
     | _ -> True
     end
*)
and demeta (want: Ty.t) (have: Ty.t) (c: Ctx.t): (Ctx.t, Ctx.t * Ty.t * Ty.t) either =
  let open Ty in
  let expand (t: Ty.t): Ty.t =
    match t.a with
    (* Should have root x thanks to `unfold`, so just need to lookup in insts *)
    | Meta x ->
        (try MetaM.find x c.insts
         with Not_found -> at ~sp:t.span (Meta x))
    | _ -> t
  in
  (* Because there's no occurs check, metas have to be expanded carefully.

     If
       ?x -> F(?x) and
       ?y -> F(?y),
     we want `unify` to show ?x ~ ?y but expanding right away would result in an
     infinite loop:
       F(?x) ~ F(?y) (* Recursion *)
       ?x ~ ?y       (* unfold ?x and ?y *)
       ?x ~ ?y       (* Incorrectly demeta ?x and ?y *)
       F(?x) ~ F(?y) (* Recursion *)
       ...

     To avoid that, we can ask unify to show F(?x) ~ F(?y) given the assumption
     ?x ~ ?y:
       F(?x) ~ F(?y) (* Recursion *)
       ?x ~ ?y       (* unfold ?x and ?y *)
       ?x ~ ?y       (* Assumption *)
       OK

     We can't just assume ?x ~ ?y by unioning the two metas together.
     Consider
       ?x -> F(?x) and
       ?y -> F(F(?y)),
     which have the same infinite expansions.

     Even when assuming ?x ~ ?y, F(?x) ~ F(F(?y)) loops:
       F(?x) ~ F(F(?y)) (* Recursion *)
       ?x ~ F(?y)       (* unfold ?x *)
       ?y ~ F(?y)       (* demeta ?y *)
       F(F(?y)) ~ F(?y) (* Recursion *)
       ...

     It's also not enough to just store equalities among metavariables.
     For example, given ?x -> F(F(?x)), ?x ~ F(?x) loops:
       ?x ~ F(?x)       (* unfold ?x *)
       ?x ~ F(?x)       (* demeta ?x *)
       F(F(?x)) ~ F(?x) (* Recursion *)
       F(?x) ~ ?x       (* unfold ?x *)
       ...

     Every time the goal is ?x ~ _ (or _ ~ ?x), we have to store an equality
     hypothesis for use later on. (Basically, we break cycles by memoizing
     `unify`.)
  *)
  match want.a, have.a, want, have with
  (* At least 1 meta: check if x and ty are already equal under an existing
     hypothesis and stop unification early if so.
     Should have root x thanks to `unfold`. *)
  | Meta x, _, _, ty | _, Meta x, ty, _ ->
    if match MetaM.find_opt x c.rec_insts with
       | Some insts -> TyS.mem ty insts
       | None -> false
    then Left c
    else (match want.a, have.a, want, have with
    (* If not equal under an existing hypothesis, add one and pass expansions
       on to `unify`. (These are split out into 2 separate cases to preserve 
       the argument positions of `want` and `have`). *)
    | Meta x, _, _, ty -> Right (add_rec_inst x ty c, expand want, expand have)
    | _, Meta x, ty, _ -> Right (add_rec_inst x ty c, expand want, expand have)
    | _ -> assert false)
  (* If there are no rooted metas here, just pass on to `unify` *)
  | _, _, _, _ -> Right (c, want, have)

(* val unify : Ty.t -> Ty.t -> Ctx.t -> Ctx.t *)
and unify ?verbose:(verbose=false) want have c =
  if verbose then 
    print_endline (Printf.sprintf "%s ~ %s -| %s" 
      (Ty.show want) (Ty.show have) (Ctx.show c))
  else ();
  let c, want = unfold want c in
  let c, have = unfold have c in
  if verbose then 
    print_endline (Printf.sprintf "unfolded: %s ~ %s -| %s"
      (Ty.show want) (Ty.show have) (Ctx.show c))
  else ();
  match demeta want have c with
  | Left c -> c
  | Right (c, want, have) -> 
      if verbose then
        print_endline (Printf.sprintf "demeta-d: %s ~ %s -| %s"
          (Ty.show want) (Ty.show have) (Ctx.show c))
      else ();
      let open Ty in
      (* Unify closed rows/columns xts ~ yts.
         The `unify` argument is used to make the function work for both rows
         and columns. *)
      let unify_closed unify xts yts c =
        (* Compute mapping from field names to (types in xts * types in yts) *)
        let xyts =
          NameM.merge
            (fun x ms mt -> match ms, mt with
             (* The set of field names must match exactly *)
             | Some s, Some t -> Some (s, t)
             | None, _ ->
                 raise (Mismatch (c, want, have, Some (Printf.sprintf
                   "former is missing field/variant %s" (Name.show x))))
             | _, None ->
                 raise (Mismatch (c, want, have, Some (Printf.sprintf
                   "latter is missing field/variant %s" (Name.show x)))))
            xts yts
        in
        (* Must have s ~ t for each pair of types s, t in xyts *)
        NameM.fold (fun _ (s, t) c -> unify s t c) xyts c
      in
      (* Unify open rows/columns xts ~ yts, where x and y are metas available
         for instantiation. stuck x /\ stuck y thanks to `unfold`.
         The `unify` argument is used to make the function work for both rows
         and columns.
         
         Return:
           - An updated c yielded by unifying all types corresponding to
             field names present in both xts and yts
           - `left : row/col NameM.t`: field names & types present only in xts
           - `right : row/col NameM.t`: field names & types present only in yts
      *)
      let unify_open unify xts yts c =
        let xyts =
          NameM.merge
            (fun x ms mt -> match ms, mt with
             | Some s, Some t -> Some (Right (s, t))
             | Some s, None -> Some (Left (Left (x, s)))
             | None, Some t -> Some (Left (Right (x, t)))
             | None, None -> None)
            xts yts
        in
        NameM.fold
          (fun _ res (c, lefts, rights) -> match res with
           | Right (s, t) -> (unify s t c, lefts, rights)
           | Left (Left (x, s)) -> (c, NameM.add x s lefts, rights)
           | Left (Right (x, t)) -> (c, lefts, NameM.add x t rights))
          xyts (c, NameM.empty, NameM.empty)
      in
      (* Unify the two rows l ~ r. `unify` and `build` are used to make the
         function work for both rows and columns. *)
      let unify_rows (type a) (unify: a -> a -> _) (build: a Ty.row -> Ty.ty)
        (l: a Ty.row) (r: a Ty.row) c: Ctx.t
      =
        let simple_mismatch (mx: Name.t option) (my: Name.t option) c =
          let show = function
            | Some x -> "row " ^ Name.show x
            | None -> "empty row" 
          in
          raise (Mismatch (c, want, have, Some (Printf.sprintf
            "expected %s but got row %s" (show mx) (show my))))
        in
        (* x_fields; ?x ~ y_fields; r where
           - r is not a meta
           - xts contains fields in x_fields and not y_fields
           - yts contains fields in y_fields and not x_fields
           - which_missing indicates (in english) whether `want` ("former") or
             `have` ("latter") is the one that is missing fields (for error
             messages)
        *)
        let one_open
          (xts: a NameM.t) (x: Meta.t) (yts: a NameM.t) (r: a Ty.row)
          (which_missing: string) (c: Ctx.t): Ctx.t
        =
          match () with
          (* r is not a meta, so if there are extra xts, the two record types
             can't be unified *)
          | _ when not (NameM.is_empty xts) ->
              let keys m = List.map fst (NameM.bindings m) in
              let show = Show.show_set NameS.elements Name.show in
              raise (Mismatch (c, want, have, Some (Printf.sprintf
                "%s is missing field/variant(s) %s"
                which_missing (show (NameS.of_list (keys xts))))))
          (* xts and yts match perfectly (there are no extra yts) *)
          | _ when NameM.is_empty yts -> add_inst x (at (build r)) c
          (* There are extra yts *)
          | _ -> add_inst x (at (build (Cons (yts, r)))) c
        in
        match l, r with
        (* 2 closed rows *)
        | Cons (xts, Nil), Cons (yts, Nil) -> unify_closed unify xts yts c
        | Cons (xts, Closed x), Cons (yts, Closed y) ->
            if not (Name.equal x y) then
              simple_mismatch (Some x) (Some y) c
            else
              unify_closed unify xts yts c
        (* stuck x /\ stuck y thanks to `unfold`.
           Thus, if the metas are equal, xts and yts must match exactly *)
        | Cons (xts, Open x), Cons (yts, Open y) when Meta.equal x y ->
            unify_closed unify xts yts c
        (* Rigid row variables never unify with empty rows *)
        | Cons (_, Nil), Cons (_, Closed x) -> simple_mismatch None (Some x) c
        | Cons (_, Closed x), Cons (_, Nil) -> simple_mismatch (Some x) None c
        (* 2 open rows *)
        | Cons (xts, Open x), Cons (yts, Open y) ->
            (* Generate fresh metas x' and y', instantiate
                 x -> rights; x'
                 y -> lefts; y',
               and add the constraint x' ~ y'.
               (As an optimization, only generate the fresh meta & instantiate
               if left/right is nonempty.)
            *)
            let c, lefts, rights = unify_open unify xts yts c in
            let add_inst' x row c =
              if NameM.is_empty row then (c, x) else 
              let x' = Meta.fresh () in
              (add_inst x (at (build (Cons (row, Open x')))) c, x')
            in
            let c, x' = add_inst' x rights c in
            let c, y' = add_inst' y lefts c in
            union x' y' c
        (* 1 open row *)
        | Cons (xts, Open x), Cons (yts, r) ->
            let c, xts, yts = unify_open unify xts yts c in
            one_open xts x yts r "latter" c
        | Cons (xts, r), Cons (yts, Open y) ->
            let c, xts, yts = unify_open unify xts yts c in
            one_open yts y xts r "former" c
        (* Nested Cons should be impossible thanks to `unfold` *)
        | Cons (_, Cons _), Cons (_, Cons _)
        (* Nil | Closed _ | Open _ should be impossible (kind error) *)
        | _, _ -> assert false
      in
      match want.a, have.a, want, have with
      (* All metas should be stuck thanks to `unfold` and `demeta`.
         If 2 metas, add an equality constraint *)
      | Meta x, Meta y, _, _ -> union x y c
      (* If 1 meta, add instantiation *)
      | Meta x, _, _, ty | _, Meta x, ty, _ -> add_inst x ty c
      (* Rec and Sum should be stuck thanks to `unfold`. *)
      | Rec l, Rec r, _, _ -> unify_rows unify build_row l r c
      | Sum l, Sum r, _, _ -> unify_rows unify_list build_col l r c
      (* App should be impossible thanks to `unfold` *)
      | App _, _, _, _ | _, App _, _, _ -> assert false
      (* Straightforward recursive cases *)
      | Lit s, Lit t, _, _ when Name.equal s t -> c
      | Var x, Var y, _, _ when Name.equal x y -> c
      | Ptr (x, s), Ptr (y, t), _, _ | Lbl (x, s), Lbl (y, t), _, _ ->
          c |> unify (at x) (at y) |> unify s t
      | Fun (ss, q), Fun (ts, r), _, _ ->
          let open List in
          if length ss <> length ts then
            raise (Mismatch (c, want, have, Some (Printf.sprintf
              "former expects %d arguments while the latter expects %d"
              (length ss) (length ts))))
          else
            c |> unify q r |> unify_list ss ts
      | _ -> raise (Mismatch (c, want, have, None))

and unify_list wants haves c =
  let open List in
  fold_left (fun c (s, t) -> unify s t c) c (combine wants haves)

(* val check : Expr.t -> Ty.t -> Ctx.t -> Ctx.t *)
let rec check_expr expr ty c =
  let c, t = infer_expr expr c in 
  unify ty t c

(* val infer : Expr.t -> Ctx.t -> Ctx.t * Ty.t *)
and infer_expr e c =
  let open Ty in
  let open Expr in
  let lit t = at (Lit (Name.id t)) in
  let sp = e.span in
  match e.a with
  | Int _ -> (c, lit "i64")
  | Float _ -> (c, lit "f64")
  | Str _s -> failwith "todo"
  | Var x -> (c, lookup sp x c)
  | Ref (x, e) ->
      let c, t = infer_expr e c in
      (trap sp x c, at ~sp (Ptr (Meta x, t)))
  | Ind (e, i) ->
      let c, ti = infer_expr i c in
      let c = unify (lit "u64") ti c in
      let c, te = infer_expr e c in
      let r = at ~sp (Meta (Meta.fresh ())) in
      let expected = at ~sp (Ptr (Meta (Meta.fresh ()), r)) in
      (unify expected te c, r)
  | Ann (e, t) -> (check_expr e t c, t)
  | Defer (_e, _x, _t, _e1) -> failwith "todo"
  | Inj (x, es) ->
      let c, ts = infer_list es c in
      (c, at ~sp (Sum (Cons (NameM.singleton x.a ts, Open (Meta.fresh ())))))
  | Case (e, pes) ->
      let r = at ~sp (Meta (Meta.fresh ())) in
      let c, xts, wildcard =
        List.fold_left
          (fun (c, xts, wildcard) (p, e) ->
             match p with
             | Some (ctr, yts) ->
                 let c, _ = Ctx.locally c (fun c ->
                   let c =
                     List.fold_left
                       (fun c (y, t) -> extend y.a t c)
                       c yts
                   in
                   (check_expr e r c, ()))
                 in
                 (c, NameM.add ctr.a (List.map snd yts) xts, wildcard)
             | None ->
                 let c, _ =
                   Ctx.locally c (fun c -> (c |> check_expr e r, ()))
                 in
                 (c, xts, true))
          (c, NameM.empty, false) pes
      in
      let te =
        let col = if wildcard then Open (Meta.fresh ()) else Nil in
        at ~sp:e.span (Sum (Cons (xts, col)))
      in
      (check_expr e te c, r)
  | Proj (e, x) ->
      let c, t = infer_expr e c in
      let r = at (Meta (Meta.fresh ())) in
      let expected =
        at ~sp (Ty.Rec (Cons (
          NameM.singleton x.a r,
          Open (Meta.fresh ()))))
      in
      (unify expected t c, r)
  | Rec xes ->
      let c, xts =
        NameM.fold 
          (fun x e (c, xts) ->
             let c, t = infer_expr e c in
             (c, NameM.add x t xts))
          xes (c, NameM.empty)
      in
      (c, at ~sp (Ty.Rec (Cons (xts, Nil))))
  | App (f, es) ->
      let c, tf = infer_expr f c in
      let targs = List.map (fun e -> at ~sp:e.span (Meta (Meta.fresh ()))) es in
      let tret = at ~sp (Meta (Meta.fresh ())) in
      let fab = at ~sp:f.span (Fun (targs, tret)) in
      let c = unify fab tf c in
      let c, ts = infer_list es c in
      (unify_list targs ts c, tret)
  | GApp (f, x, es) ->
      let (ty_binds: NameS.t NameM.t), tf = lookup_global sp f.a c in
      let (m: Meta.t NameM.t), tf = eapply tf in
      let new_metas = m |> NameM.bindings |> List.map snd |> MetaS.of_list in
      let c = add_poly_inst x new_metas c in
      let targs = List.map (fun e -> at ~sp:e.span (Meta (Meta.fresh ()))) es in
      let tret = at ~sp (Meta (Meta.fresh ())) in
      let fab = at ~sp:f.span (Fun (targs, tret)) in
      let c = unify fab tf c in
      let c, ts = infer_list es c in
      let c = unify_list targs ts c in
      let c =
        NameM.fold (fun x -> NameS.fold (require (NameM.find x m))) ty_binds c
      in
      (c, tret)
  | Sus (l, x, e) ->
      let te = at ~sp (Meta (Meta.fresh ())) in
      let tl = at ~sp (Lbl (Meta x, te)) in
      Ctx.locally c (fun c ->
        let c = c |> Ctx.extend l.a tl |> trap sp x |> check_expr e te in
        (c, tl))
  | Res e ->
      let r = at ~sp (Meta (Meta.fresh ())) in
      let expected = at ~sp (Lbl (Meta (Meta.fresh ()), r)) in
      (check_expr e expected c, r)
  | Let (x, t, r, e) ->
      let c = check_expr r t c in
      Ctx.locally c (fun c -> c
        |> Ctx.extend x.a t
        |> infer_expr e)
  | Set (l, r, e) ->
      let c, tl = infer_expr l c in
      let c, tr = infer_expr r c in
      let c = unify tl tr c in
      infer_expr e c

and infer_list es c =
  let c, ts = 
    List.fold_left
      (fun (c, ts) e ->
         let c, t = infer_expr e c in 
         (c, t :: ts))
      (c, [])
      es
  in
  (c, List.rev ts)
