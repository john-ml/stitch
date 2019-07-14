open Misc

(* Abstract syntax of the core language *)

module Ty = struct

type hole = Open of Meta.t | Closed of Name.t

type row = hole option

type t = ty Node.t

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
let tup ts = Node.at (Sum (NameM.singleton (Name.internal "tuple") ts, None))

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
    match ty.Node.a with
    | Lit t -> Name.show t
    | Var x -> Name.show x
    | Ptr (r, t) -> join ["*("; show_hole r; ", "; go t; ")"]
    | Lbl (r, t) -> join ["..("; show_hole r; ", "; go t; ")"]
    | Rec (xs, None) -> join ["{"; fields go xs; "}"]
    | Rec (xs, r) -> join ["{"; fields go xs; "; "; show_row r; "}"]
    | Sum (xs, r) ->
        (match NameM.bindings xs, r with
         | [x, ts], None when Name.(equal x (internal "tuple")) ->
             join ["("; tuple ts; ")"]
         | _ -> join ["<"; fields tuple xs ; "; "; show_row r; ">"])
    | Fun (ts, r) -> join ["("; tuple ts; ") -> "; go r]
    | App (f, ts) -> join [Name.show f; "("; tuple ts; ")"]
    | Meta x -> Meta.show x
  in go

end (* Ty *)

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
  | (* &e                  *) Ref of t
  | (* e[e]                *) Ind of t * t
  | (* e as t              *) Ann of t * Ty.t
  | (* e defer x as t -> e *) Defer of t * Name.t * Ty.t * t
  | (* x@(e, ..)           *) Inj of Name.t Node.t * t list
  | (* case e {p -> e, ..} *) Case of t * (pat * t) list
  | (* e.x                 *) Proj of t * Name.t Node.t
  | (* {x = e, ..}         *) Rec of t NameM.t
  | (* e(e, ..)            *) App of t * t list
  | (* l: e                *) Sus of Name.t Node.t * t
  | (* ..e                 *) Res of t
  | (* x as t = e; e       *) Let of Name.t Node.t * Ty.t * t * t
  | (* e := e; e           *) Set of t * t * t

(* Convenience constructors with no location info *)

let mk_int i = Node.at (Int i)
let mk_float x = Node.at (Float x)
let mk_str s = Node.at (Str s)
let mk_var x = Node.at (Var (Name.id x))
let mk_ref e = Node.at (Ref e)
let mk_ind e i = Node.at (Ind (e, i))
let mk_ann e t = Node.at (Ann (e, t))
let mk_defer e x t e1 = Node.at (Defer (e, x, t, e1))
let mk_inj x es = Node.at (Inj (Node.at (Name.id x), es))
let mk_case x arms = Node.at (Case (x, arms))
let mk_proj x field = Node.at (Proj (x, Node.at (Name.id field)))
let mk_rec xes = Node.at (Rec (NameM.of_list xes))
let mk_app f xs = Node.at (App (f, xs))
let mk_sus l e = Node.at (Sus (Node.at (Name.id l), e))
let mk_res e = Node.at (Res e)
let mk_let x t e1 e = Node.at (Let (Node.at (Name.id x), t, e1, e))
let mk_set l r e = Node.at (Set (l, r, e))

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

let bop (a: t) (op: bop Node.t) (b: t): t =
  let open Node in
  let open Span in
  at ~sp:(join a.span b.span) (
    App (
      at ~sp:op.span (Var (bop_name op.a)),
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
  let ptr_a = at (Ptr (Closed (id "R"), a)) in
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

let rec doc (e: t): Doc.t =
  let open Doc in
  let open Node in
  match e.a with
  | Int i -> Lit (string_of_int i)
  | Float x -> Lit (string_of_float x)
  | Str s -> Lit (String.escaped s)
  | Var x -> Lit (Name.show x)
  | Ref t -> Lit "&" +++ doc t
  | Ind (e, i) -> doc e +++ Lit "[" +++ doc i +++ Lit "]"
  | Ann (e, t) -> doc e +++ Lit (" as " ^ Ty.show t)
  | Defer (e, x, t, e1) ->
      doc e +++ Lit (" " ^ Name.show x ^ " as " ^ Ty.show t ^ " -> ") +++ doc e1
  | Inj (x, es) ->
      let ds = List.map doc es in
      if List.for_all single_line ds then
        Name.show x.a >>> par (horzs ds)
      else
        Name.show x.a >>> vpar (Ind (2, verts (sep_by "," ds)))
  | Case (
      {a = App ({a = Var fbool; _}, [p]); _},
      [ Some (ptrue, []), a
      ; Some (pfalse, []), b
      ])
    when let open Name in
         equal fbool (id "__bool__")
         && equal ptrue.a (internal "true")
         && equal pfalse.a (internal "false")
    ->
       let dp = doc p in
       let da = doc a in
       let db = doc b in
       (* TODO find a better way to do this *)
       (match single_line dp, single_line da, single_line db with
        | true, true, true ->
            ("if " >>> dp) +++ (" then " >>> da) +++ (" else " >>> db)
        | true, true, false ->
            "if " ^ render dp ^ " then " ^ render da ^ " else " >>> vpar (Ind (2, db))
        | true, false, true ->
            ("if " ^ render dp ^ " then ") >>> vpar (Ind (2, da)) <<< " else " ^ render db
        | true, false, false ->
            "if " ^ render dp >>> (Lit " then ("
            --- Ind (2, da)
            --- Lit ") else ("
            --- Ind (2, db)
            --- Lit ")")
        | false, true, true ->
            "if " >>> vpar (Ind (2, dp)) <<< " then " ^ render da ^ " else " ^ render db
        | false, true, false ->
            (("if " >>> vpar (Ind (2, dp))) <<< " then " ^ render da ^ " else (")
            --- Ind (2, db) --- Lit ")"
        | false, false, true ->
            Lit "if ("
            --- Ind (2, dp)
            --- Lit ") then ("
            --- Ind (2, da)
            --- Lit ") else " <<< render db
        | false, false, false ->
            Lit "if ("
            --- Ind (2, dp)
            --- Lit ") then ("
            --- Ind (2, da)
            --- Lit ") else ("
            --- Ind (2, db)
            --- Lit ")")
  | Case (e, pes) ->
      let de = doc e in
      let ds =
        sep_by ", " (List.map (fun (p, e) -> show_pat p ^ " -> " >>> doc e) pes)
      in
      (match single_line de, List.for_all single_line ds with
       | true, true -> ("case " >>> par de <<< " ") +++ brac (horzs ds)
       | true, false ->
           "case " ^ render (par de) >>> vbrac (Ind (2, verts ds))
       | false, true ->
           vpar (Ind (2, vpar (Ind (2, de)))) <<< render (brac (horzs ds))
       | false, false ->
           Lit "case (" --- Ind (2, de) --- Lit ") {"
           --- Ind (2, verts ds) --- Lit "}")
  | Proj (e, x) ->
      let d = doc e in
      if single_line d then
        par d <<< "." ^ Name.show x.a
      else
        vpar (Ind (2, d)) <<< "." ^ Name.show x.a
  | Rec xes ->
      let open List in
      let ds =
        sep_by ", " (
          map
            (fun (x, e) -> Name.show x ^ " = " >>> doc e)
            (NameM.bindings xes))
      in
      if for_all (fun d -> single_line d) ds
      then brac (horzs ds)
      else vbrac (verts ds)
  | App (f, xs) ->
      let df = doc f in
      let ds = sep_by ", " (List.map doc xs) in
      (match single_line df, List.for_all single_line ds with
       | true, true -> par df +++ par (horzs ds)
       | true, false ->
           render (par df) >>> vpar (Ind (2, verts ds))
       | false, true ->
           vpar (Ind (2, vpar (Ind (2, df))))
           <<< render (par (horzs ds))
       | false, false ->
           vpar (Ind (2, df) --- Lit ")(" --- Ind (2, verts ds)))
  | Sus (l, e) ->
      let d = doc e in
      if single_line d then
        Name.show l.a ^ ": " >>> d
      else
        Name.show l.a ^ ": " >>> vpar (Ind (2, d))
  | Res e ->
      let d = doc e in
      if single_line d then
        ".." >>> d
      else
        ".." >>> vpar (Ind (2, d))
  | Let (x, t, r, e) ->
      (Lit (Name.show x.a ^ " as " ^ Ty.show t ^ " = ") +++ doc r <<< ";") 
      --- doc e
  | Set (l, r, e) ->
      let dl = doc l in
      if single_line dl then
        ((dl +++ Lit " := " +++ doc r) <<< ";") --- doc e
      else
        Lit "(" --- Ind (2, dl) --- (") := " >>> doc r <<< ";") --- doc e

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
