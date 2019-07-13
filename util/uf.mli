(* Persistent union-find *)
type t

(* Empty UF instance *)
val empty : t

(* Get representative *)
val find : Meta.t -> t -> Meta.t

(* Get representative and return a path-compressed instance *)
val find' : Meta.t -> t -> t * Meta.t

(* Union two representatives and perform path-compression *)
val union : Meta.t -> Meta.t -> t -> t
