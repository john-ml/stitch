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
  }

(* Uf.find' lifted to contexts *)
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

(* AppI.add lifted to contexts *)
let add_app x fxs c = {c with apps = AppI.add x fxs c.apps}

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
  "{locals = " ^ show_locals c.locals ^
  "; globals = " ^ show_globals c.globals ^
  "; aliases = " ^ show_aliases c.aliases^
  "; uf = " ^ Uf.show c.uf ^
  "; insts = " ^ show_insts c.insts ^
  "; rec_insts = " ^ show_rec_insts c.rec_insts ^
  "; apps = " ^ show_apps c.apps ^
  "}"

end

open Ctx

(* Helpers for working with rows/cols *)
let extract_row = function Ty.Rec r -> Some r | _ -> None
let extract_col = function Ty.Sum r -> Some r | _ -> None
let build_row r = Ty.Rec r
let build_col r = Ty.Sum r

exception Unbound of Span.t * Name.t
exception Mismatch of Ctx.t * Ty.t * Ty.t * string option

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
    | Cons (xts, r) ->
        (* Recursively unfold nested rows *)
        let c, r = go r c in
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
         The `unify` and `build` arguments are used to make the function work
         for both rows and columns. *)
      let unify_open unify build x y xts yts c =
        let open NameM in
        (* Collect:
           - An updated c yielded by unifying all types corresponding to
             field names present in both xts and yts
           - `left : row/col NameM.t`: field names & types present only in xts
           - `right : row/col NameM.t`: field names & types present only in yts
        *)
        let xyts =
          merge
            (fun x ms mt -> match ms, mt with
             | Some s, Some t -> Some (Right (s, t))
             | Some s, None -> Some (Left (Left (x, s)))
             | None, Some t -> Some (Left (Right (x, t)))
             | None, None -> None)
            xts yts
        in
        let c, lefts, rights =
          fold
            (fun _ res (c, lefts, rights) -> match res with
             | Right (s, t) -> (unify s t c, lefts, rights)
             | Left (Left (x, s)) -> (c, add x s lefts, rights)
             | Left (Right (x, t)) -> (c, lefts, add x t rights))
            xyts (c, empty, empty)
        in
        (* Generate fresh metas x' and y' and instantiate
             x = rights; x'
             y = lefts; y'
        *)
        let x', y' = Meta.(fresh (), fresh ()) in
        c |> add_inst x (at (build (Cons (rights, Open x'))))
          |> add_inst y (at (build (Cons (lefts, Open y'))))
      in
      match want.a, have.a, want, have with
      (* All metas should be stuck thanks to `unfold` and `demeta`.
         If 2 metas, add an equality constraint *)
      | Meta x, Meta y, _, _ -> union x y c
      (* If 1 meta, add instantiation *)
      | Meta x, _, _, ty | _, Meta x, ty, _ -> add_inst x ty c
      (* Rec and Sum should be stuck thanks to `unfold`.
         `unify_closed` for Cons cases with closed rows/cols *)
      | Rec (Cons (xts, Nil)), Rec (Cons (yts, Nil)), _, _ ->
          unify_closed unify xts yts c
      | Sum (Cons (xtss, Nil)), Sum (Cons (ytss, Nil)), _, _ ->
          unify_closed unify_list xtss ytss c
      | Rec (Cons (xts, Closed x)), Rec (Cons (yts, Closed y)), _, _
        when Name.equal x y -> unify_closed unify xts yts c
      | Sum (Cons (xtss, Closed x)), Sum (Cons (ytss, Closed y)), _, _
        when Name.equal x y -> unify_closed unify_list xtss ytss c
      (* `unify_open` for Cons cases with open rows/cols *)
      | Rec (Cons (xts, Open x)), Rec (Cons (yts, Open y)), _, _ ->
          unify_open unify build_row x y xts yts c
      | Sum (Cons (xtss, Open x)), Sum (Cons (ytss, Open y)), _, _ ->
          unify_open unify_list build_col x y xtss ytss c
      (* Nested Cons should be impossible thanks to `unfold` *)
      | Rec (Cons (_, Cons _)), Rec (Cons (_, Cons _)), _, _
      | Sum (Cons (_, Cons _)), Sum (Cons (_, Cons _)), _, _
      (* Cons (_, Nil | Closed _ | Open _) should be impossible (kind error) *)
      | Rec _, Rec _, _, _ | Sum _, Sum _, _, _ -> assert false
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
and infer_expr _e _c =
  raise Todo (* TODO *)
  (*let open Node in
  match e.a with
  |*)
