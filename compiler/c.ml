open Misc

(* Abstract syntax of C *)

module Ty = struct

type t
  = Prim of Name.t
  | (* *t                 *) Ptr of t
  | (* (t, ..) -> t       *) Fun of t list * t
  | (* struct { x t; .. } *) Struct of t NameM.t
  | (* union { x t; .. }  *) Union of t NameM.t

end

module Expr = struct

type t
  = (* inline C *) C of string
  | (* literals *) Int of int | Str of string | Float of float
  | (* x        *) Var of Name.t
  | (* e.x      *) Dot of t * Name.t
  | (* e[e]     *) Ind of t * t
  | (* e(e, ..) *) App of t * t list
  | (* ((t)e)   *) Cast of Name.t * t

end

module Body = struct

type t = stmt list

and stmt
  = (* t x;      *) Decl of Name.t * Name.t
  | (* l = r;    *) Set of Expr.t * Expr.t
  | (* (void) e; *) Void of Expr.t
  | (* return e; *) Ret of Expr.t
  | (* switch (e) { case i: .. case i: s; break; .. } *)
    Switch of Expr.t * (IntS.t * t) list

end

module Fun = struct

(* t f(t x, ..) { s } *)
type t = Name.t * Name.t * (Name.t * Name.t) list * Body.t

end
