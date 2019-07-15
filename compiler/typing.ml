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
  ; (* Instantiations for metas *) insts: Ty.t MetaM.t       
  ; (* Metas for applications   *) apps: AppI.t              
  }

(* Uf.find' lifted to contexts *)
let find' x c = let uf, x = Uf.find' x c.uf in ({c with uf = uf}, x)

(* Uf.union lifted to contexts *)
let union x y c = {c with uf = Uf.union x y c.uf}

(* MetaM.add lifted to contexts *)
let add_inst x t c = let insts = MetaM.add x t c.insts in {c with insts = insts}

(* AppI.add lifted to contexts *)
let add_app x fxs c = let apps = AppI.add x fxs c.apps in {c with apps = apps}

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
    | Lit _ | Meta _ | AMeta _ -> t
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

(* Replace variables with metas as given by the mapping `m`.
   Replace variables not in `m` with fresh metas.
   Return updated Name -> Meta mapping. *)
let eapply ?m:(m=NameM.empty): Ty.t -> Meta.t NameM.t * Ty.t =
  let open Ty in
  let open NameM in
  let rec go m t = 
    let m, ta =
      match t.a with
      | Lit _ | Meta _ | AMeta _ -> (m, t.a)
      | Var x when mem x m -> (m, Meta (find x m))
      | Var x -> let y = Meta.fresh () in (add x y m, Meta y)
      | Ptr (x, t) -> let m, t = go m t in (m, Ptr (x, t))
      | Lbl (x, t) -> let m, t = go m t in (m, Lbl (x, t))
      | Rec _ | Sum _ -> raise Todo (* TODO *)
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
   forall let (c, r) := unfold c ty,
     let stuck x := x = Uf.find x c.uf /\ ~ MetaM.mem x c.insts in
     let fix stuck_row r :=
       match r with
       | Nil | Closed _ -> True
       | Open x -> stuck x
       | Cons (_, Cons _) -> False
       | Cons (_, r) -> stuck_row r
       end
     in
     match r with
     | Meta x -> stuck x
     | App _ -> False
     | Rec r | Sum r -> stuck_row r
     | _ -> True
     end *)
let rec unfold (c: ctx) (ty: Ty.t): ctx * Ty.t =
  let open Ty in
  let wrap ta = at ~sp:ty.span ta in
  match ty.a with
  (* Unfold metas to expose a non-Meta constructor at root, if possible *)
  | Meta x ->
      let c, x = find' x c in
      (c, try MetaM.find x c.insts with Not_found -> wrap (Meta x))
  (* Unfold row variables *)
  | Rec r ->
      let c, r = unfold_row extract_row build_row c r in
      (c, wrap (Rec r))
  | Sum r ->
      let c, r = unfold_row extract_col build_col c r in
      (c, wrap (Sum r))
  (* Turn alias applications into AMetas *)
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
       | Some x -> (c, wrap (AMeta x))
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
           (* Associate y with its unfolding and return AMeta y *)
           let c = add_inst y ty c in
           (c, wrap (AMeta y)))
  | _ -> c, ty

(* val unify : ctx -> Ty.t -> Ty.t -> ctx *)
and unify c want have =
  let c, want = unfold c want in
  let c, have = unfold c have in
  let open Ty in
  match want.a, have.a, want, have with
  (* If both stuck metas, add equality constraint *)
  | Meta x, Meta y, _, _ -> union x y c
  (* If 1 stuck meta, add instantiation *)
  | Meta x, _, _, ty | _, Meta x, ty, _ -> {c with insts = MetaM.add x ty c.insts}
  | Rec _, Rec _, _, _ -> raise Todo (* TODO *)
  | Sum _, Sum _, _, _ -> raise Todo (* TODO *)
  | App _, _, _, _ | _, App _, _, _ -> raise Todo
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
        fold_left (fun c (s, t) -> unify c s t) (unify c q r) (combine ss ts)
  | _ -> raise (Mismatch (c, want, have, None))

(* val check : ctx -> Expr.t -> Ty.t -> ctx *)
let rec check c expr ty = unify c ty (infer c expr)

(* val infer : ctx -> Expr.t -> Ty.t *)
and infer _c _e =
  raise Todo (* TODO *)
  (*let open Node in
  match e.a with
  |*)
