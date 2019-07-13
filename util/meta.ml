(* A metavariable *)
type t = int

let compare : t -> t -> int = (-)

let equal : t -> t -> bool = (=)

(* Generate fresh metavariable *)
let fresh : unit -> t =
  let i = ref 0 in
  fun () -> i := !i + 1; !i

let show x = "?" ^ string_of_int x
