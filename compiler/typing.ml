open Misc
open Node
open Syntax

exception Unbound of Span.t * Name.t
exception Mismatch of Span.t * Ty.t * Ty.t

(* Type-checking context *)
type ctx =
  { env: Ty.t NameM.t         (* Γ *)
  ; ty_env: Ty.alias NameM.t
  ; uf: Uf.t                  (* For solving equality constraints on metas *)
  ; hyps: Uf.t
  ; insts: Ty.t MetaM.t       (* Concrete instantiations for metas *)
  }

(* val zonk_whnf : ctx -> Ty.t -> Ty.t *)
let zonk_whnf c ty =
  let open Ty in
  match ty.a with
  | Meta x ->
      let x = Uf.find x c.uf in
      (try MetaM.find x c.insts
       with Not_found -> at ~sp:ty.span (Meta x))
  | _ -> ty

(*
type expr
  = (* integer             *) Int of int
  | (* float               *) Float of float
  | (* string              *) Str of string
  | (* x, y, etc.          *) Var of Name.t
  | (* &e                  *) Ref of t
  | (* e[e]                *) Ind of t * t
  | (* e as t              *) Ann of t * Ty.t
  | (* e defer x -> e      *) Defer of t * Name.t * t
  | (* x@(e, ..)           *) Inj of Name.t Node.t * t list
  | (* case e {p -> e, ..} *) Case of t * (pat * t) list
  | (* e.x                 *) Proj of t * Name.t Node.t
  | (* {x = e, ..}         *) Rec of t NameM.t
  | (* e(e, ..)            *) App of t * t list
  | (* l: e                *) Sus of Name.t Node.t * t
  | (* ..e                 *) Res of t
  | (* x as t = e; e       *) Let of Name.t Node.t * Ty.t * t * t
  | (* e := e; e           *) Set of t * t * t


and ty
  = (* i32, i64, etc.     *) Lit of Name.t
  | (* A, B, etc.         *) Var of Name.t
  | (* *(?A, T)           *) Ptr of hole * t
  | (* ..(?A, T)          *) Lbl of hole * t
  | (* {x T, ..; R}       *) Rec of t NameM.t * row
  | (* <x (T, ..), ..; R> *) Sum of t list NameM.t * row
  | (* (T, ..) -> T       *) Fun of t list * t
  | (* F(T, ..)           *) App of Name.t * t list
  | (* ?A                 *) Meta of Meta.t
*)

(* val unify : ctx -> Ty.t -> Ty.t -> ctx *)
let unify _c _want _have = raise Todo

(* val check : ctx -> Expr.t -> Ty.t -> ctx *)
let check c expr ty =
  let module E = Expr in
  let module T = Ty in
  let open Name in
  let ty = zonk_whnf c ty in
  match expr.a, ty.a with
  | E.Int _, T.Lit t ->
      let integral_types =
        List.map id ["i8"; "i16"; "i32"; "i64"; "u8"; "u16"; "u32"; "u64"]
      in
      if NameS.(mem t (of_list integral_types)) then c else
      raise (Mismatch (expr.span, ty, at (T.Lit (id "integral type"))))
  | E.Var x, _ ->
      (match NameM.find_opt x c.env with
       | Some have -> unify c ty have
       | None -> raise (Unbound (expr.span, x)))
  | E.Ref e, T.Ptr (_, t) -> check c e t (* TODO: do we need to unify the lifetime? *)
  | _ -> raise Todo

(* val infer : ctx -> Expr.t -> Ty.t *)
and infer _env _uf _e =
  raise Todo
  (*let open Node in
  match e.a with
  |*)
