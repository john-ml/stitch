open Misc

(* Abstract syntax of the core language *)

module Ty = struct

type 'a row
  = (* ε          *) Nil
  | (* R          *) Closed of Name.t
  | (* ?R         *) Open of Meta.t
  | (* x a, ..; r *) Cons of 'a NameM.t * 'a row

type t = ty Node.t

and ty
  = (* i32, i64, etc.     *) Lit of Name.t
  | (* A, B, etc.         *) Var of Name.t
  | (* *(?A, T)           *) Ptr of ty * t
  | (* ..(?A, T)          *) Lbl of ty * t
  | (* {x T, ..; R}       *) Rec of t row
  | (* <x (T, ..), ..; R> *) Sum of t list row
  | (* (T, ..) -> T       *) Fun of t list * t
  | (* F(T, ..)           *) App of Name.t * t list
  | (* ?A                 *) Meta of Meta.t

(* Tuple types become closed single-constructor sums *)
let tup ts = Node.at (Sum (Cons (NameM.singleton (Name.internal "tuple") ts, Nil)))

(* A series of type-level bindings: A trait .., .. *)
type binds = NameS.t NameM.t

(* Type alias (type-level lambda) *)
type alias = Name.t list * t

(* Polymorphic type *)
type poly = binds * t

let binds (ts: (Name.t * Name.t list) list): binds =
  NameM.of_list (List.map (fun (x, ts) -> (x, NameS.of_list ts)) ts)

(* Initial environment for type-level primitives *)
let env: alias NameM.t =
  let open NameM in
  let open Node in
  let open Name in
  of_list
    [ (* type bool = <__true (), __false ()> *)
      id "bool", ([], at (Sum (Cons (of_list 
        [internal "true", []; internal "false", []],
        Nil))))
    ]

let show =
  let open String in
  let join = concat "" in
  let fields show_elt xs =
    concat ", " (List.map (fun (x, t) ->
      concat " " [Name.show x; show_elt t]) (NameM.bindings xs))
  in
  let rec show_row go = function
    | Nil -> ""
    | Closed x -> Name.show x
    | Open x -> Meta.show x
    | Cons (xs, Nil) -> fields go xs
    | Cons (xs, r) -> join [fields go xs; "; "; show_row go r]
  in
  let rec go ty =
    let tuple ts = concat ", " (List.map go ts) in
    match ty.Node.a with
    | Lit t -> Name.show t
    | Var x -> Name.show x
    | Ptr (r, t) -> join ["*("; go (Node.at r); ", "; go t; ")"]
    | Lbl (r, t) -> join ["..("; go (Node.at r); ", "; go t; ")"]
    | Rec r -> join ["{"; show_row go r; "}"]
    | Sum r -> join ["<"; show_row tuple r; ">"]
    | Fun (ts, r) -> join ["("; tuple ts; ") -> "; go r]
    | App (f, ts) -> join [Name.show f; "("; tuple ts; ")"]
    | Meta x -> Meta.show x
  in go

let rec compare_row compare l r =
  let ctr = function
    | Nil -> 0
    | Closed _ -> 1
    | Open _ -> 2
    | Cons _ -> 3
  in
  match ctr l - ctr r with
  | 0 ->
      (match l, r with
       | Nil, Nil -> 0
       | Closed x, Closed y -> Name.compare x y
       | Open x, Open y -> Meta.compare x y
       | Cons (xts, l), Cons (yts, r) ->
           (match NameM.compare compare xts yts with
            | 0 -> compare_row compare l r
            | cmp -> cmp)
       | _, _ -> assert false)
  | cmp -> cmp

let compare l r =
  let open Node in
  let ctr = function
    | Lit _ -> 0
    | Var _ -> 1
    | Ptr _ -> 2
    | Lbl _ -> 3
    | Rec _ -> 4
    | Sum _ -> 5
    | Fun _ -> 6
    | App _ -> 7
    | Meta _ -> 8
  in
  let rec go l r =
    match ctr l - ctr r with
    | 0 ->
        (match l, r with
         | Lit x, Lit y | Var x, Var y -> Name.compare x y
         | Ptr (q, s), Ptr (r, t) | Lbl (q, s), Lbl (r, t) -> 
             Compare.pair go compare (q, s) (r, t)
         | Rec l, Rec r -> compare_row compare l r
         | Sum l, Sum r -> compare_row (Compare.list compare) l r
         | Fun (ss, q), Fun (ts, r) ->
             Compare.(pair (list compare) compare) (ss, q) (ts, r)
         | App (g, ss), App (f, ts) ->
             Compare.(pair Name.compare (list compare)) (g, ss) (f, ts)
         | Meta x, Meta y -> Meta.compare x y
         | _, _ -> assert false)
    | cmp -> cmp
  in
  go l.a r.a

module Notation = struct
  let ( !? ) _ = Node.at (Meta (Meta.fresh ()))
end

end (* Ty *)

module TyS = Set.Make(Ty)

module Expr = struct

(* Term-level bindings: x as t, .. *)
type binds = (Name.t Node.t * Ty.t) list

(* Pattern in a case expression: ctr (x as t, ..) | _ *)
type pat = (Name.t Node.t * binds) option

type t = expr Node.t

and expr
  = (* integer             *) Int of int
  | (* float               *) Float of float
  | (* string              *) Str of string
  | (* x, y, etc.          *) Var of Name.t
  | (* &e                  *) Ref of Meta.t * t
  | (* e[e]                *) Ind of t * t
  | (* e as t              *) Ann of t * Ty.t
  | (* e defer x as t -> e *) Defer of t * Name.t * Ty.t * t
  | (* x@(e, ..)           *) Inj of Name.t Node.t * t list
  | (* case e {p -> e, ..} *) Case of t * (pat * t) list
  | (* e.x                 *) Proj of t * Name.t Node.t
  | (* {x = e, ..}         *) Rec of t NameM.t
  | (* e(e, ..)            *) App of t * t list
  | (* f@[t, ..](e, ..)    *) GApp of Name.t Node.t * Meta.t * t list
  | (* l: e                *) Sus of Name.t Node.t * Meta.t * t
  | (* ..e                 *) Res of t
  | (* x as t = e; e       *) Let of Name.t Node.t * Ty.t * t * t
  | (* e := e; e           *) Set of t * t * t

(* Convenience constructors with no location info *)

let mk_int i = Node.at (Int i)
let mk_float x = Node.at (Float x)
let mk_str s = Node.at (Str s)
let mk_var x = Node.at (Var (Name.id x))
let mk_ref x e = Node.at (Ref (x, e))
let mk_ind e i = Node.at (Ind (e, i))
let mk_ann e t = Node.at (Ann (e, t))
let mk_defer e x t e1 = Node.at (Defer (e, x, t, e1))
let mk_inj x es = Node.at (Inj (Node.at (Name.id x), es))
let mk_case x arms = Node.at (Case (x, arms))
let mk_proj x field = Node.at (Proj (x, Node.at (Name.id field)))
let mk_rec xes = Node.at (Rec (NameM.of_list xes))
let mk_app f xs = Node.at (App (f, xs))
let mk_gapp f x xs = Node.at (GApp (f, x, xs))
let mk_sus l x e = Node.at (Sus (Node.at (Name.id l), x, e))
let mk_res e = Node.at (Res e)
let mk_let x t e1 e = Node.at (Let (Node.at (Name.id x), t, e1, e))
let mk_set l r e = Node.at (Set (l, r, e))

module Notation = struct
  let ( ~$ ) = mk_var
  let ( ~! ) = mk_int
  let ( ~% ) = mk_float
  let ( *: ) x t = (x, t)
  let ( -= ) (x, t) = mk_let x t
  let ( += ) = mk_set
  let ( *. ) = mk_proj
  let ( *:: ) = mk_ann
  let ( *$ ) = mk_app
  let ( *@ ) (x, ts) = mk_gapp x ts
  let ( *.. ) = mk_ind
end

type uop
  = (* -       *) Neg
  | (* !       *) Not 
  | (* new del *) New | Del

(* Unary operators become unary function calls *)

let uop_name: uop -> Name.t = let open Name in function
  | Neg -> id "__neg__"
  | Not -> id "__not__"
  | New -> id "__new__"
  | Del -> id "__del__"

let uop (op: uop Node.t) (e: t): t =
  let open Node in
  let open Span in
  at ~sp:(join op.span e.span) (
    GApp (
      at ~sp:op.span (uop_name op.a),
      Meta.fresh (),
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

let bop (a: t) (op: bop Node.t) (b: t): t =
  let open Node in
  let open Span in
  at ~sp:(join a.span b.span) (
    GApp (
      at ~sp:op.span (bop_name op.a),
      Meta.fresh (),
      [a; b]))

(* *p becomes p[0] *)
let deref (sp: Span.t) (p: t): t =
  let open Node in
  at ~sp (Ind (p, at ~sp (Int 0)))

(* Boolean literal becomes nullary injection *)
let mk_blit name sp =
  let open Node in
  let open Name in
  at ~sp (
    Ann (
      at ~sp (Inj (at ~sp (internal name), [])),
      at ~sp (Ty.Lit (id "bool"))))
let btrue: Span.t -> t = mk_blit "true"
let bfalse: Span.t -> t = mk_blit "false"

(* if .. then .. else becomes case expression *)
let ite (sp: Span.t) (p: t) (a: t) (b: t): t =
  let open Node in
  let open Name in
  at ~sp (
    Case (
      at ~sp:p.span (App (at ~sp:p.span (Var (id "__bool__")), [p])),
      [ Some (at (internal "true"), []), a
      ; Some (at (internal "false"), []), b
      ]))

(* && and || become if .. then .. else *)
let aand (a: t) (b: t): t = ite Node.(Span.join a.span b.span) a b a
let oor (a: t) (b: t): t = ite Node.(Span.join a.span b.span) a a b

(* when becomes nested if .. then .. else *)
let cond (sp: Span.t) (arms: (t * t) list) (last: t): t =
  { (List.fold_right
      (fun (p, e) r -> ite Node.(Span.join p.span r.span) p e r)
      arms last)
    with span = sp
  }

(* Tuples become injections into closed sums *)
let tup (sp: Span.t) (es: t list) =
  let open Node in
  let open Ty in
  at ~sp (
    Ann (
      at ~sp (Inj (at ~sp (Name.internal "tuple"), es)), 
      tup (List.map (fun e -> at ~sp:e.span (Meta (Meta.fresh ()))) es)))

(* Tuple-destructuring let becomes case expression *)
let lets (sp: Span.t) (xs: binds) (e1: t) (e: t): t =
  let open Node in
  at ~sp (Case (e1, [Some (at (Name.internal "tuple"), xs), e]))

(* e defer f becomes e defer x -> f(x) *)
let defer (e: t) (f: Name.t Node.t): t =
  let open Node in
  let open Span in
  let x = Name.fresh () in
  let m = Meta.fresh () in
  at ~sp:(join e.span f.span) (
    Defer (e, x, at ~sp:f.span (Ty.Meta m), at ~sp:f.span (
      App (
        at ~sp:f.span (Var f.a),
        [at ~sp:f.span (Var x)]))))

(* Initial environment for term-level primitives *)
let env: Ty.poly NameM.t =
  let open NameM in
  let open Node in
  let open Ty in
  let open Name in
  let a = at (Var (id "A")) in
  let ptr_a = at (Ptr (Var (id "R"), a)) in
  let tbool = at (Var (id "bool")) in
  let un trait = (binds [id "A", [id trait]], at (Fun ([a], a))) in
  let bin trait = (binds [id "A", [id trait]], at (Fun ([a; a], a))) in
  of_list
    [ uop_name Neg, un "num"
    ; uop_name Not, un "log"
    ; uop_name New, (binds [id "R", []; id "A", []], at (Fun ([a], ptr_a)))
    ; uop_name Del, (binds [id "R", []; id "A", []], at (Fun ([ptr_a], a)))
    ; bop_name Add, bin "num"
    ; bop_name Sub, bin "num"
    ; bop_name Mul, bin "num"
    ; bop_name Div, bin "num"
    ; bop_name And, bin "log"
    ; bop_name Or, bin "log"
    ; id "__bool__", (binds [id "A", [id "bool"]], at (Fun ([a], tbool)))
    ]

let show_pat: pat -> string = let open Node in function
  | None -> "_"
  | Some (x, binds) ->
      Name.show x.a
      ^ "("
      ^ String.concat ", "
          (List.map (fun (x, t) -> Name.show x.a ^ " as " ^ Ty.show t) binds)
      ^ ")"

let rec pp ff (e: t) =
  let open Format in
  let printf s = fprintf ff s in
  let comma_sep pp = pp_print_list ~pp_sep:(fun ff _ -> fprintf ff ",@;<1 2>") pp in
  match e.a with
  | Int i -> printf "%d" i
  | Float x -> printf "%f" x
  | Str s -> printf "%s" (String.escaped s)
  | Var x -> printf "%s" (Name.show x)
  | Ref (x, t) -> printf "&(%s) %a" (Meta.show x) pp t
  | Ind (e, i) -> printf "@[<2>(%a[%a])@]" pp e pp i
  | Ann (e, t) -> printf "@[<2>(%a as %s)@]" pp e (Ty.show t)
  | Defer (e, x, t, e1) ->
      printf "@[(%a defer %s as %s -> %a)@]"
        pp e (Name.show x) (Ty.show t) pp e1
  | Inj (x, es) ->
      printf "@[%s(@,%a)@]"
        (Name.show x.a)
        (pp_print_list ~pp_sep:pp_print_cut pp) es
  | Case (e, pes) ->
      printf "@[<v>case %a {@;<0 2>%a@,}@]"
        pp e
        (comma_sep (fun ff (p, e) ->
          fprintf ff "@[%s ->@;<1 2>@[%a@]@]" (show_pat p) pp e))
        pes
  | Proj (e, x) -> printf "%a.%s" pp e (Name.show x.a)
  | Rec xes ->
      printf "@ {@[<hv>{@;<0 2>%a@,}@]"
        (comma_sep (fun ff (x, e) ->
          fprintf ff "@[<hv>%s = @;<1 2>@[%a@]@]" (Name.show x) pp e))
        (NameM.bindings xes)
  | App (f, xs) -> printf "@[<hv>@[%a@](@;<0 1>@[%a@])@]" pp f (comma_sep pp) xs
  | GApp (f, x, xs) ->
      printf "@[<hv>%s:[%s](@;<0 1>@[%a@])@]"
        (Name.show f.a) (Meta.show x) (comma_sep pp) xs
  | Sus (l, x, e) -> printf "@[%s(%s): %a@]" (Name.show l.a) (Meta.show x) pp e
  | Res e -> printf "..%a" pp e
  | Let (x, t, r, e) ->
      printf "@[<v>@[%s as %s =@;<1 2>%a;@]@;@[%a@]@]"
        (Name.show x.a) (Ty.show t) pp r pp e
  | Set (l, r, e) -> printf "@[<v>@[%a@ :=@;<1 2>%a;@]@;@[%a@]@]" pp l pp r pp e

end (* Expr *)

(* \ (A, ..) -> t *)
type tdecl = NameS.t * Ty.t

(* [A trait .., ..](x as t, ..) t = e *)
type fdecl = Ty.binds * Expr.binds * Ty.t * Expr.t

(* Just need to record the names of the constants associated with each trait *)
type trait = NameS.t NameM.t

(* impl(A trait .., ..) t trait { ... } *)
type impl =
  { prereqs: Ty.binds
  ; ty: Name.t
  ; trait: Name.t
  ; fdecls: fdecl NameM.t
    (* TODO: constants? desugar constants A to functions unit -> A? *)
  }

type prgm =
  { fdecls: fdecl NameM.t
  ; tdecls: tdecl NameM.t
  ; impls: impl list
  }
