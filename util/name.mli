(* A name *)
type t

val compare : t -> t -> int

val equal : t -> t -> bool

(* Make a name from a string *)
val id : string -> t

(* Make a name from a string that will never conflict with user-defined names *)
val internal : string -> t

(* A guaranteed fresh internal name *)
val fresh : unit -> t

val show : t -> string
