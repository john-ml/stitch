exception Todo

module Span = struct
  type loc = int * int
  type t = (loc * loc) option
  let show_loc (r, c) = string_of_int r ^ ":" ^ string_of_int c
  (* Compute the smallest region that contains both of the given regions *)
  let join a b =
    match a, b with
    | a, None | None, a -> a
    | Some ((lr1, lc1), (lr2, lc2)), Some ((rr1, rc1), (rr2, rc2)) ->
        Some ((min lr1 rr1, min lc1 rc1), (max lr2 rr2, max lc2 rc2))
  let show = function
    | Some (rc1, rc2) -> show_loc rc1 ^ "-" ^ show_loc rc2
    | None -> "<no location>"
end

module Node = struct
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

module IntS = Set.Make(struct
  type t = int
  let compare = (-)
end)
