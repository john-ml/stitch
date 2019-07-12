exception Todo

module Name = struct
  include String
end

module Meta = struct
  type t = int
  let compare = (-)
end

module Span = struct
  type rc = int * int
  type t = (rc * rc) option
end

module Loc = struct
  type 'a t = {a: 'a; span: Span.t}
  let mk ?(span=None) a = {a; span}
end

module NameM = Map.Make(Name)
module MetaM = Map.Make(Meta)
module NameS = Set.Make(Name)
module MetaS = Set.Make(Meta)

module Ty = struct

type row = Open of Meta.t | Closed of Name.t

type t = ty Loc.t

and ty
  = (* i32, i64, etc.     *) Lit of Name.t
  | (* A, B, etc.         *) Var of Name.t
  | (* &(?A, T)           *) Ptr of Meta.t * t
  | (* ..(?A, T)          *) Lbl of Meta.t * t
  | (* (T, ..)            *) Tup of t list
  | (* {x T, ..; R}       *) Rec of t NameM.t * row
  | (* <x (T, ..), ..; R> *) Sum of t list NameM.t * row
  | (* (T, ..) -> T       *) Fun of t list * t
  | (* F(T, ..)           *) App of Name.t * t list
  | (* ?A                 *) Meta of Meta.t

let rec desugared (t : t) : bool =
  match t.a with
  | Tup _ -> false
  | Lit _ | Var _ -> true
  | Ptr (_, t) | Lbl (_, t) -> desugared t

end (* Ty *)

type binds = (* x as t, .. *) (Name.t Loc.t * Ty.t) list

let binds_desugared (xs : binds) : bool =
  List.for_all (fun (_, t) -> Ty.desugared t) xs

module Expr = struct

type uop
  = Neg | Not 
  | Ref | Deref
  | New | Del

type bop
  = Add | Mul | Sub | Div
  | And | Or | AAnd | OOr

type pat = (* ctr (x as t, ..) | _ *) (Name.t * binds) option

type t = expr Loc.t

and expr
  = (* x, y, etc.          *) Var of Name.t
  | (* e op e              *) Bop of t * bop Loc.t * t
  | (* op e                *) Uop of uop Loc.t * t
  | (* x@e                 *) Inj of Name.t Loc.t * t
  | (* e.x                 *) Dot of t * Name.t Loc.t
  | (* e[e]                *) Ind of t * t
  | (* (e, ..)             *) Tup of t list
  | (* e(e, ..)            *) App of t * t list
  | (* l: e                *) Sus of Name.t Loc.t * t
  | (* ..e                 *) Exe of t
  | (* (x as t, ..) = e; e *) Let of binds * t * t
  | (* e := e; e           *) Set of t * t * t
  | (* if e then e else e  *) Ite of t * t * t
  | (* when {e -> e, ..}   *) When of (t * t) list
  | (* case {p -> e, ..}   *) Case of (pat * t) list

let rec desugared (e : t) : bool =
  match e.Loc.a with
  | Ite _ | When _ | Tup _ -> false
  | Let (xs, _, _) when List.length xs <> 1 -> false
  | Let (xs, e1, e) -> binds_desugared xs && desugared e1 && desugared e
  | Var _ -> true
  | Uop (_, a) | Inj (_, a) | Dot (a, _) | Sus (_, a) | Exe a -> desugared a
  | Bop (a, _, b) | Ind (a, b) -> desugared a && desugared b
  | App (e, es) -> desugared e && List.for_all desugared es
  | Set (a, b, c) -> desugared a && desugared b && desugared c
  | Case arms ->
      List.for_all (fun (p, e) ->
        match p with
        | Some (_, xs) -> binds_desugared xs && desugared e
        | None -> desugared e)
        arms

let desugar (e : t) : t = raise Todo

end (* Expr *)

type ty_binds = (* A trait .., .. *) NameS.t NameM.t

type tdecl = (* (A, ..) -> t *) NameS.t * Ty.t

type fdecl
  = (* [A trait .., ..](x as t, ..) t = e *) ty_binds * binds * Ty.t * Expr.t

type prgm =
  { fdecls: fdecl NameM.t
  ; tdecls: tdecl NameM.t
  ; impls: int
  }
