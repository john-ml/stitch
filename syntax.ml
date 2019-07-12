exception Todo

module Name = struct
  include String
  let id s = s
  let internal s = "0"^s
  let fresh =
    let i = ref 0 in
    fun () -> i := !i + 1; string_of_int !i
  let show s = s
end

module Meta = struct
  type t = int
  let compare = (-)
  let fresh =
    let i = ref 0 in
    fun () -> i := !i + 1; !i
  let show x = "?" ^ string_of_int x
end

module Span = struct
  type rc = int * int
  type t = (rc * rc) option
  let join a b =
    match a, b with
    | a, None | None, a -> a
    | Some ((lr1, lc1), (lr2, lc2)), Some ((rr1, rc1), (rr2, rc2)) ->
        Some ((min lr1 rr1, min lc1 rc1), (max lr2 rr2, max lc2 rc2))
  let show = function
    | Some (rc1, rc2) ->
        let show_rc (r, c) = string_of_int r ^ ":" ^ string_of_int c in
        show_rc rc1 ^ "-" ^ show_rc rc2
    | None -> "<no location>"
end

module Loc = struct
  type 'a t = {a: 'a; span: Span.t}
  let at ?sp:(span=None) a = {a; span}
  let no_loc () = None
end

module NameM = Map.Make(Name)
module MetaM = Map.Make(Meta)
module NameS = Set.Make(Name)
module MetaS = Set.Make(Meta)

module Ty = struct

type row = Unsolved of Meta.t | Solved of Name.t | Closed

type t = ty Loc.t

and ty
  = (* i32, i64, etc.     *) Lit of Name.t
  | (* A, B, etc.         *) Var of Name.t
  | (* *(?A, T)           *) Ptr of Meta.t * t
  | (* ..(?A, T)          *) Lbl of Meta.t * t
  | (* {x T, ..; R}       *) Rec of t NameM.t * row
  | (* <x (T, ..), ..; R> *) Sum of t list NameM.t * row
  | (* (T, ..) -> T       *) Fun of t list * t
  | (* F(T, ..)           *) App of Name.t * t list
  | (* ?A                 *) Meta of Meta.t

(* Tuple types become closed single-constructor sums *)
let tup ts = Loc.at (Sum (NameM.singleton (Name.internal "tuple") ts, Closed))

let show_row = function
  | Unsolved x -> Meta.show x
  | Solved x -> Name.show x
  | Closed -> ""

let show =
  let open String in
  let join = concat "" in
  let fields show_elt xs =
    concat ", " (List.map (fun (x, t) ->
      concat " " [Name.show x; show_elt t]) (NameM.bindings xs))
  in
  let rec go ty =
    let tuple ts = concat ", " (List.map go ts) in
    match ty.Loc.a with
    | Lit t -> Name.show t
    | Var x -> Name.show x
    | Ptr (r, t) -> join ["*("; Meta.show r; ", "; go t; ")"]
    | Lbl (r, t) -> join ["..("; Meta.show r; ", "; go t; ")"]
    | Rec (xs, Closed) -> join ["{"; fields go xs; "}"]
    | Rec (xs, r) -> join ["{"; fields go xs; "; "; show_row r; "}"]
    | Sum (xs, r) ->
        (match NameM.bindings xs, r with
         | [x, ts], Closed when String.equal x (Name.internal "tuple") ->
             join ["("; tuple ts; ")"]
         | _ -> join ["<"; fields tuple xs ; "; "; show_row r; ">"])
    | Fun (ts, r) -> join ["("; tuple ts; ") -> "; go r]
    | App (f, ts) -> join [Name.show f; "("; tuple ts; ")"]
    | Meta x -> Meta.show x
  in go

end (* Ty *)

type binds = (* x as t, .. *) (Name.t Loc.t * Ty.t) list

module Expr = struct

type pat = (* ctr (x as t, ..) | _ *) (Name.t Loc.t * binds) option

type t = expr Loc.t

and expr
  = (* x, y, etc.          *) Var of Name.t
  | (* e as t              *) Ann of t * Ty.t
  | (* e defer x -> e      *) Defer of t * Name.t * t
  | (* x@(e, ..)           *) Inj of Name.t Loc.t * t list
  | (* case e {p -> e, ..} *) Case of t * (pat * t) list
  | (* e.x                 *) Proj of t * Name.t Loc.t
  | (* {x = e, ..}         *) Rec of t NameM.t
  | (* e(e, ..)            *) App of t * t list
  | (* l: e                *) Sus of Name.t Loc.t * t
  | (* ..e                 *) Exe of t
  | (* x as t = e; e       *) Let of Name.t Loc.t * Ty.t * t * t
  | (* e := e; e           *) Set of t * t * t

type uop
  = (* -       *) Neg
  | (* !       *) Not 
  | (* & *     *) Ref | Deref
  | (* new del *) New | Del

(* Unary operators become unary function calls *)
let uop (op: uop Loc.t) (e: t): t =
  let open Loc in
  let open Span in
  let open Name in
  let name =
    match op.a with
    | Neg -> id "__neg__"
    | Not -> id "__not__"
    | Ref -> id "__ref__"
    | Deref -> id "__deref__"
    | New -> id "__new__"
    | Del -> id "__del__"
  in
  at ~sp:(join op.span e.span) (App (at ~sp:op.span (Var name), [e]))

type bop
  = (* + - * / *) Add | Mul | Sub | Div
  | (* & |     *) And | Or

(* Binary operators become binary function calls *)
let uop (a: t) (op: bop Loc.t) (b: t): t =
  let open Loc in
  let open Span in
  let open Name in
  let name =
    match op.a with
    | Add -> id "__add__"
    | Mul -> id "__mul__"
    | Sub -> id "__sub__"
    | Div -> id "__div__"
    | And -> id "__and__"
    | Or -> id "__or__"
  in
  at ~sp:(join a.span b.span) (App (at ~sp:op.span (Var name), [a; b]))

(* Array index becomes function call *)
let ind (e: t) (i: t): t =
  let open Loc in
  let open Span in
  at ~sp:(join e.span i.span) (App (
    at ~sp:i.span (Var (Name.id "__index__")),
    [i]))

(* Boolean literal becomes nullary injection *)
let mk_blit name sp =
  let open Loc in
  let open Name in
  at ~sp (Ann (
    at ~sp (Inj (at ~sp (internal name), [])),
    at ~sp (Ty.Lit (id "bool"))))
let btrue: Span.t -> t = mk_blit "true"
let bfalse: Span.t -> t = mk_blit "false"

(* if .. then .. else becomes case expression *)
let ite (sp: Span.t) (p: t) (a: t) (b: t): t =
  let open Loc in
  let open Name in
  let open Span in
  at ~sp (Case (
    at ~sp:p.span (App (at ~sp:p.span (Var (id "__bool__")), [p])),
    [ Some (at (internal "true"), []), a
    ; Some (at (internal "false"), []), b
    ]))

(* && and || become if .. then .. else *)
let aand (a: t) (b: t): t = ite Loc.(Span.join a.span b.span) a b a
let oor (a: t) (b: t): t = ite Loc.(Span.join a.span b.span) a a b

(* when becomes nested if .. then .. else *)
let cond (sp: Span.t) (arms: (t * t) list) (last: t) =
  List.fold_right
    (fun (p, e) r -> ite Loc.(Span.join p.span r.span) p e r)
    arms last

(* Tuples become injections into closed sums *)
let tup (sp: Span.t) (es: t list) =
  let open Span in
  let open Loc in
  let open Ty in
  at ~sp (Ann (
    at ~sp (Inj (at ~sp (Name.internal "tuple"), es)), 
    tup (List.map (fun e -> at ~sp:e.span (Meta (Meta.fresh ()))) es)))

(* Tuple-destructuring let becomes case expression *)
let lets (sp: Span.t) (xs: binds) (e1: t) (e: t): t =
  let open Loc in
  at ~sp (Case (e1, [Some (at (Name.internal "tuple"), xs), e]))

end (* Expr *)

type ty_binds = (* A trait .., .. *) NameS.t NameM.t

type tdecl = (* (A, ..) -> t *) NameS.t * Ty.t

type fdecl
  = (* [A trait .., ..](x as t, ..) t = e *) ty_binds * binds * Ty.t * Expr.t

(* TODO *)
type impl
  = (* impl(t trait .., ..) t trait { ... } *)
    { prereqs: (Ty.t list * NameS.t) list
    ; ty: Name.t
    ; trait: Name.t
    }

type prgm =
  { fdecls: fdecl NameM.t
  ; tdecls: tdecl NameM.t
  ; impls: int (* TODO *)
  }
