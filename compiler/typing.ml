open Misc
open Node
open Syntax

(* Each type application f(?X, ..) is associated with a unique meta *)
module AppI = Iso (Meta) (OrdPair (Name) (OrdList (Meta)))

(* Type-checking context *)
type ctx =
  { (* Values / monomorphic     *) locals: Ty.t NameM.t      
  ; (* Global / polymorphic     *) globals: Ty.poly NameM.t  
  ; (* Type aliases             *) aliases: Ty.alias NameM.t 
  ; (* Equalities among metas   *) uf: Uf.t                  
  ; (* Coinductive hypotheses   *) rec_uf: Uf.t                  
  ; (* Instantiations for metas *) insts: Ty.t MetaM.t       
  ; (* Metas for applications   *) apps: AppI.t              
  }

(* Uf.find' lifted to contexts *)
let find' x c = let uf, x = Uf.find' x c.uf in ({c with uf = uf}, x)
let rec_find' x c =
  let rec_uf, x = Uf.find' x c.rec_uf in 
  ({c with rec_uf = rec_uf}, x)

(* Uf.union lifted to contexts *)
let union x y c = {c with uf = Uf.union x y c.uf}
let rec_union x y c = {c with rec_uf = Uf.union x y c.rec_uf}

(* Uf.equal' lifted to contexts *)
let rec_equal' x y c =
  let rec_uf, p = Uf.equal' x y c.rec_uf in
  ({c with rec_uf = rec_uf}, p)

(* MetaM.add lifted to contexts *)
let add_inst x t c = {c with insts = MetaM.add x t c.insts}

(* AppI.add lifted to contexts *)
let add_app x fxs c = {c with apps = AppI.add x fxs c.apps}

(* Helpers for working with rows/cols *)
let extract_row = function Ty.Rec r -> Some r | _ -> None
let extract_col = function Ty.Sum r -> Some r | _ -> None
let build_row r = Ty.Rec r
let build_col r = Ty.Sum r

exception Unbound of Span.t * Name.t
exception Mismatch of ctx * Ty.t * Ty.t * string option

let subst: (Span.t -> Name.t -> Ty.t) -> Ty.t -> Ty.t = fun f ->
  let open Ty in
  let rec go_row extract sp = function
    | Nil as r -> r
    | Open _ as r -> r
    | Cons (m, r) -> Cons (m, go_row extract sp r)
    | Closed x ->
        match extract (f sp x).a with
        | Some r -> r
        | None -> assert false
  in
  let rec go t = 
    let wrap ta = {t with a = ta} in
    match t.a with
    | Lit _ | Meta _ -> t
    | Var x -> f t.span x
    | Ptr (x, t) -> wrap (Ptr (x, go t))
    | Lbl (x, t) -> wrap (Ptr (x, go t))
    | Rec r -> wrap (build_row (go_row extract_row t.span r))
    | Sum r -> wrap (build_col (go_row extract_col t.span r))
    | Fun (ts, r) -> wrap (Fun (List.map go ts, go r))
    | App (f, ts) -> wrap (App (f, List.map go ts))
  in
  go

let apply: Ty.alias -> Ty.t list -> Ty.t = fun (args, t) ts ->
  assert List.(length args = length ts);
  let m = NameM.of_list (List.combine args ts) in
  subst (fun _ x -> NameM.find x m) t

(* Helper for `eapply`. *)
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
  let open NameM in
  let rec go m t = 
    let m, ta =
      match t.a with
      | Lit _ | Meta _ -> (m, t.a)
      | Var x when mem x m -> (m, Meta (find x m))
      | Var x -> let y = Meta.fresh () in (add x y m, Meta y)
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

let unfold_row extract build: ctx -> 'a Ty.row -> ctx * 'a Ty.row =
  let rec go c r =
    let open Ty in
    match r with
    | Nil | Closed _ -> (c, r)
    | Open x ->
        (* Unfold row-metas *)
        let c, x = find' x c in
        (try let t = MetaM.find x c.insts in
             match extract t.a with
             | Some r ->
                 let c, r = go c r in
                 (* 'Path compression' on row-metas *)
                 let c = add_inst x (at ~sp:t.span (build r)) c in
                 (c, r)
             | _ -> assert false
         with Not_found -> (c, Open x))
    | Cons (xts, r) ->
        (* Recursively unfold nested rows *)
        let c, r = go c r in
        let xtsr =
          (* Collapse Cons (xts, Cons (yts, r)) -> Cons (xts ∪ yts, r) *)
          match r with
          | Cons (yts, r) ->
              let join _ mss mts =
                match mss, mts with
                | None, None -> None
                | Some ts, None | None, Some ts -> Some ts
                | Some _, Some _ -> assert false
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
let rec unfold (c: ctx) (ty: Ty.t): ctx * Ty.t =
  let open Ty in
  let wrap ta = at ~sp:ty.span ta in
  match ty.a with
  (* Root metas *)
  | Meta x -> let c, x = find' x c in (c, wrap (Meta x))
  (* Unfold row/col variables *)
  | Rec r -> let c, r = unfold_row extract_row build_row c r in (c, wrap (Rec r))
  | Sum r -> let c, r = unfold_row extract_col build_col c r in (c, wrap (Sum r))
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
                  let c = unify c (at ~sp:t.span (Meta x)) t in
                  (c, x :: xs))
               (c, [])
               (combine args ts)
           in
           (* Associate App (f, xs) with a fresh meta y *)
           let y = Meta.fresh () in
           let c = add_app y (f, xs) c in
           (* Associate y with its unfolding and return Meta y *)
           let c = add_inst y ty c in
           (c, wrap (Meta y)))
  | _ -> c, ty

(* Unfold metas to expose a non-Meta constructor at root, if possible,
   and check if any coinductive hypotheses hold.

   forall updated wants and haves as r,
     match r with
     | Meta x -> stuck x
     | _ -> True
     end
*)
and demeta (c: ctx) (want: Ty.t) (have: Ty.t): (ctx, ctx * Ty.t * Ty.t) either =
  let open Ty in
  let expand (t: Ty.t): Ty.t =
    match t.a with
    (* Should have root x thanks to `unfold`, so just need to lookup in insts *)
    | Meta x ->
        (try MetaM.find x c.insts
         with Not_found -> at ~sp:t.span (Meta x))
    | _ -> t
  in
  match want.a, have.a with
  (* Should have root x /\ root y thanks to `unfold`.
     Because there's no occurs check, metas have to be expanded carefully.

     If
       ?x -> F(?x)
       ?y -> F(?y)
     we want `unify` to show ?x ~ ?y but expanding right away would result in an
     infinite loop:
       F(?x) ~ F(?y) (* Recursion *)
       ?x ~ ?y       (* unfold ?x and ?y *)
       ?x ~ ?y       (* Incorrectly demeta ?x and ?y *)
       F(?x) ~ F(?y) (* Recursion *)
       .
       .
       .

     To avoid that, we can ask unify to show F(?x) ~ F(?y) given the assumption
     ?x ~ ?y:
       F(?x) ~ F(?y) (* Recursion *)
       ?x ~ ?y       (* unfold ?x and ?y *)
       ?x ~ ?y       (* Assumption *)
       OK

     Also, we can't just assume ?x ~ ?y by unioning the two metas together.
     Consider:
       ?x -> F(?x) and
       ?y -> F(F(?y)),
     which have the same infinite expansions.

     Unioning ?x ~ ?y and asking unify to show F(?x) ~ F(F(?y)) yields:
       F(?x) ~ F(F(?y)) (* Recursion *)
       ?x ~ F(?y)       (* unfold ?x *)
       ?y ~ F(?y)       (* demeta ?y *)
       F(F(?y)) ~ F(?y) (* Recursion *)
       F(?y) ~ ?y       (* unfold ?y *)
       F(?y) ~ ?y       (* demeta ?y *)
       F(?y) ~ F(F(?y)) (* Recursion *)
       .
       .
       .
     which never terminates.

     To avoid this, the equality hypotheses have to be stored in a separate UF
     instance and checked here. If the current goal turns out to correspond to
     an equality hypothesis, unification can end now and we don't call `unify`
     with expanded metas.
  *)
  | Meta x, Meta y ->
      (* Check if x and y are already equal under an existing hypothesis *)
      (match rec_equal' x y c with
       | c, true -> Left c
       | c, false ->
           (* If not, assume ?x ~ ?y and pass their expansions on to `unify` *)
           let c = rec_union x y c in 
           Right (c, expand want, expand have))
  | _, _ -> Right (c, expand want, expand have)

(* val unify : ctx -> Ty.t -> Ty.t -> ctx *)
and unify c want have =
  let c, want = unfold c want in
  let c, have = unfold c have in
  match demeta c want have with
  | Left c -> c
  | Right (c, want, have) -> 
      let open Ty in
      match want.a, have.a, want, have with
      (* All metas should be stuck thanks to `unfold` and `demeta`.
         If 2 metas, add an equality constraint. *)
      | Meta x, Meta y, _, _ -> union x y c
      (* If 1 meta, add instantiation *)
      | Meta x, _, _, ty | _, Meta x, ty, _ -> add_inst x ty c
      (* Rec and Sum should be stuck thanks to `unfold` *)
      | Rec _, Rec _, _, _ -> raise Todo (* TODO *)
      | Sum _, Sum _, _, _ -> raise Todo (* TODO *)
      (* App should be impossible thanks to `unfold` *)
      | App _, _, _, _ | _, App _, _, _ -> assert false
      (* Straightforward recursive cases *)
      | Lit s, Lit t, _, _ when Name.equal s t -> c
      | Var x, Var y, _, _ when Name.equal x y -> c
      | Ptr (x, s), Ptr (y, t), _, _ | Lbl (x, s), Lbl (y, t), _, _ ->
          unify (unify c (at x) (at y)) s t
      | Fun (ss, q), Fun (ts, r), _, _ ->
          let open List in
          if length ss <> length ts then
            raise (Mismatch (c, want, have, Some (Printf.sprintf
              "(former expects %d arguments while the latter expects %d)"
              (length ss) (length ts))))
          else
            fold_left
              (fun c (s, t) -> unify c s t) 
              (unify c q r)
              (combine ss ts)
      | _ -> raise (Mismatch (c, want, have, None))

(* val check : ctx -> Expr.t -> Ty.t -> ctx *)
let rec check c expr ty = unify c ty (infer c expr)

(* val infer : ctx -> Expr.t -> Ty.t *)
and infer _c _e =
  raise Todo (* TODO *)
  (*let open Node in
  match e.a with
  |*)
