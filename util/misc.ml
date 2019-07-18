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
  include Map.Make (Name)
  let of_list kvs =
    List.fold_left
      (fun m (k, v) -> add k v m)
      empty kvs
end
module MetaM = Map.Make (Meta)
module NameS = Set.Make (Name)
module MetaS = Set.Make (Meta)

module IntS = Set.Make (struct
  type t = int
  let compare = (-)
end)

module Compare = struct
  let pair compare_a compare_b (x1, y1) (x2, y2) =
    match compare_a x1 x2 with
    | 0 -> compare_b y1 y2
    | r -> r
  let rec list compare xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | x :: xs, y :: ys ->
        match compare x y with
        | 0 -> list compare xs ys
        | r -> r
end

module OrdList (Ord: Set.OrderedType) = struct
  type t = Ord.t list
  let compare xs ys = Compare.list Ord.compare xs ys
end

module OrdPair (OrdA: Set.OrderedType) (OrdB: Set.OrderedType) = struct
  type t = OrdA.t * OrdB.t
  let compare p1 p2 = Compare.pair OrdA.compare OrdB.compare p1 p2
end

module Iso (OrdA: Set.OrderedType) (OrdB: Set.OrderedType) = struct
  module FromA = Map.Make(OrdA)
  module FromB = Map.Make(OrdB)
  type a = OrdA.t
  type b = OrdB.t
  type t = b FromA.t * a FromB.t
  let empty = (FromA.empty, FromB.empty)
  let add a b (ab, ba) = (FromA.add a b ab, FromB.add b a ba)
  let memA a (ab, _) = FromA.mem a ab
  let memB b (_, ba) = FromB.mem b ba
  let findA a (ab, _) = FromA.find a ab
  let findB b (_, ba) = FromB.find b ba
  let findA_opt a (ab, _) = FromA.find_opt a ab
  let findB_opt b (_, ba) = FromB.find_opt b ba
  let equalities (ab, _) = FromA.bindings ab
end

module Option = struct
  type 'a t = 'a option
  let map f = function
    | Some x -> Some (f x)
    | None as m -> m
  let bind f = function
    | Some x -> f x
    | None as m -> m
end

type ('a, 'b) either = Left of 'a | Right of 'b

module type Show = sig
  type t
  val show : t -> string
end

module Show = struct
  let show_list show_elt xs = 
    "[" ^ String.concat "; " (List.map show_elt xs) ^ "]"
  let show_pair show_a show_b (a, b) =
    "(" ^ show_a a ^ ", " ^ show_b b ^ ")"
  let show_set elements show_elt s =
    "{" ^ String.concat ", " (List.map show_elt (elements s)) ^ "}"
  let show_map bindings show_k show_v m =
    let kvs = 
      List.map 
        (fun (k, v) -> show_k k ^ " -> " ^ show_v v)
        (bindings m)
    in
    "{" ^ String.concat ", " kvs ^ "}"
  let show_iso equalities show_a show_b i =
    let abs = 
      List.map 
        (fun (a, b) -> show_a a ^ " <-> " ^ show_b b)
        (equalities i)
    in
    "{" ^ String.concat ", " abs ^ "}"
end
