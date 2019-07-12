exception Todo

module Name = struct
  include String
  (* Make a name from a string *)
  let id s = s
  (* A mangled copy that will never conflict with user-defined names *)
  let internal s = "0"^s
  (* A fresh internal name *)
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
  (* Compute the smallest region that contains both of the given regions *)
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

module NameM = struct
  include Map.Make(Name)
  let of_list kvs =
    List.fold_left
      (fun m (k, v) -> add k v m)
      empty kvs
end
module MetaM = Map.Make(Meta)
module NameS = Set.Make(Name)
module MetaS = Set.Make(Meta)

(* Abstract syntax of the core language *)

module Ty = struct

type hole = Open of Meta.t | Closed of Name.t

type row = hole option

type t = ty Loc.t

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

(* Tuple types become closed single-constructor sums *)
let tup ts = Loc.at (Sum (NameM.singleton (Name.internal "tuple") ts, None))

(* A series of type-level bindings: A trait .., .. *)
type binds = NameS.t NameM.t

(* Type alias (type-level lambda) *)
type alias = Name.t list * t

(* Polymorphic type *)
type poly = binds * t

let binds (ts : (Name.t * Name.t list) list) : binds =
  NameM.of_list (List.map (fun (x, ts) -> (x, NameS.of_list ts)) ts)

(* Initial environment for type-level primitives *)
let env : alias NameM.t =
  let open NameM in
  let open Loc in
  let open Name in
  of_list
    [ (* type bool = <__true (), __false ()> *)
      id "bool", ([], at (Sum (of_list 
        [internal "true", []; internal "false", []],
        None)))
    ]

let show_hole = function
  | Open x -> Meta.show x
  | Closed x -> Name.show x

let show_row = function
  | Some h -> show_hole h
  | None -> ""

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
    | Ptr (r, t) -> join ["*("; show_hole r; ", "; go t; ")"]
    | Lbl (r, t) -> join ["..("; show_hole r; ", "; go t; ")"]
    | Rec (xs, None) -> join ["{"; fields go xs; "}"]
    | Rec (xs, r) -> join ["{"; fields go xs; "; "; show_row r; "}"]
    | Sum (xs, r) ->
        (match NameM.bindings xs, r with
         | [x, ts], None when String.equal x (Name.internal "tuple") ->
             join ["("; tuple ts; ")"]
         | _ -> join ["<"; fields tuple xs ; "; "; show_row r; ">"])
    | Fun (ts, r) -> join ["("; tuple ts; ") -> "; go r]
    | App (f, ts) -> join [Name.show f; "("; tuple ts; ")"]
    | Meta x -> Meta.show x
  in go

end (* Ty *)

module Expr = struct

(* Term-level bindings: x as t, .. *)
type binds = (Name.t Loc.t * Ty.t) list

(* Pattern in a case expression: ctr (x as t, ..) | _ *)
type pat = (Name.t Loc.t * binds) option

type t = expr Loc.t

and expr
  = (* x, y, etc.          *) Var of Name.t
  | (* &e                  *) Ref of t
  | (* e as t              *) Ann of t * Ty.t
  | (* e defer x -> e      *) Defer of t * Name.t * t
  | (* x@(e, ..)           *) Inj of Name.t Loc.t * t list
  | (* case e {p -> e, ..} *) Case of t * (pat * t) list
  | (* e.x                 *) Proj of t * Name.t Loc.t
  | (* {x = e, ..}         *) Rec of t NameM.t
  | (* e(e, ..)            *) App of t * t list
  | (* l: e                *) Sus of Name.t Loc.t * t
  | (* ..e                 *) Res of t
  | (* x as t = e; e       *) Let of Name.t Loc.t * Ty.t * t * t
  | (* e := e; e           *) Set of t * t * t

type uop
  = (* -       *) Neg
  | (* !       *) Not 
  | (* *       *) Deref
  | (* new del *) New | Del

(* Unary operators become unary function calls *)

let uop_name: uop -> Name.t = let open Name in function
  | Neg -> id "__neg__"
  | Not -> id "__not__"
  | Deref -> id "__deref__"
  | New -> id "__new__"
  | Del -> id "__del__"

let uop (op: uop Loc.t) (e: t): t =
  let open Loc in
  let open Span in
  let open Name in
  at ~sp:(join op.span e.span) (
    App (
      at ~sp:op.span (Var (uop_name op.a)),
      [e]))

(* Binary operators become binary function calls *)

type bop
  = (* + - * / *) Add | Mul | Sub | Div
  | (* & |     *) And | Or

let bop_name: bop -> Name.t = let open Name in function
  | Add -> id "__add__"
  | Mul -> id "__mul__"
  | Sub -> id "__sub__"
  | Div -> id "__div__"
  | And -> id "__and__"
  | Or -> id "__or__"

let bop (a: t) (op: bop Loc.t) (b: t): t =
  let open Loc in
  let open Span in
  let open Name in
  at ~sp:(join a.span b.span) (
    App (
      at ~sp:op.span (Var (bop_name op.a)),
      [a; b]))

(* Array index becomes function call *)
let ind (e: t) (i: t): t =
  let open Loc in
  let open Span in
  at ~sp:(join e.span i.span) (
    App (
      at ~sp:i.span (Var (Name.id "__index__")),
      [i]))

(* Boolean literal becomes nullary injection *)
let mk_blit name sp =
  let open Loc in
  let open Name in
  at ~sp (
    Ann (
      at ~sp (Inj (at ~sp (internal name), [])),
      at ~sp (Ty.Lit (id "bool"))))
let btrue: Span.t -> t = mk_blit "true"
let bfalse: Span.t -> t = mk_blit "false"

(* if .. then .. else becomes case expression *)
let ite (sp: Span.t) (p: t) (a: t) (b: t): t =
  let open Loc in
  let open Name in
  let open Span in
  at ~sp (
    Case (
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
  at ~sp (
    Ann (
      at ~sp (Inj (at ~sp (Name.internal "tuple"), es)), 
      tup (List.map (fun e -> at ~sp:e.span (Meta (Meta.fresh ()))) es)))

(* Tuple-destructuring let becomes case expression *)
let lets (sp: Span.t) (xs: binds) (e1: t) (e: t): t =
  let open Loc in
  at ~sp (Case (e1, [Some (at (Name.internal "tuple"), xs), e]))

(* e defer f becomes e defer x -> f(x) *)
let defer (e: t) (f: Name.t Loc.t): t =
  let open Loc in
  let open Span in
  let x = Name.fresh () in
  at ~sp:(join e.span f.span) (
    Defer (e, x, at ~sp:f.span (
      App (
        at ~sp:f.span (Var f.a),
        [at ~sp:f.span (Var x)]))))

(* Initial environment for term-level primitives *)
let env : Ty.poly NameM.t =
  let open NameM in
  let open Loc in
  let open Ty in
  let open Name in
  let a = at (Var (id "A")) in
  let u64 = at (Lit (id "u64")) in
  let ptr_a = at (Ptr (Closed (id "R"), a)) in
  let tbool = at (Var (id "bool")) in
  let un trait = (binds [id "A", [id trait]], at (Fun ([a], a))) in
  let bin trait = (binds [id "A", [id trait]], at (Fun ([a; a], a))) in
  of_list
    [ uop_name Neg, un "num"
    ; uop_name Not, un "log"
    ; uop_name Deref, (binds [id "R", []; id "A", []], at (Fun ([ptr_a], a)))
    ; uop_name New, (binds [id "R", []; id "A", []], at (Fun ([a], ptr_a)))
    ; uop_name Del, (binds [id "R", []; id "A", []], at (Fun ([ptr_a], a)))
    ; bop_name Add, bin "num"
    ; bop_name Sub, bin "num"
    ; bop_name Mul, bin "num"
    ; bop_name Div, bin "num"
    ; bop_name And, bin "log"
    ; bop_name Or, bin "log"
    ; id "__index__", (binds [id "A", []], at (Fun ([ptr_a; u64], a)))
    ; id "__bool__", (binds [id "A", [id "bool"]], at (Fun ([a], tbool)))
    ]

end (* Expr *)

type tdecl = (* (A, ..) -> t *) NameS.t * Ty.t

type fdecl
  = (* [A trait .., ..](x as t, ..) t = e *)
    Ty.binds * Expr.binds * Ty.t * Expr.t

(* TODO *)
type impl
  = (* impl(A trait .., ..) t trait { ... } *)
    { prereqs: Ty.binds
    ; ty: Name.t
    ; trait: Name.t
    }

type prgm =
  { fdecls: fdecl NameM.t
  ; tdecls: tdecl NameM.t
  ; impls: int (* TODO *)
  }
