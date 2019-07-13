type t = String.t

let compare = String.compare

let equal = String.equal

let id s = s

let internal s = "0"^s

let fresh =
  let i = ref 0 in
  fun () -> i := !i + 1; string_of_int !i

let show s = s
