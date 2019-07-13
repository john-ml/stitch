type t
  = Empty
  | Lit of string (* A string without newlines *)
  | Horz of t * t (* Horizontal concatenation *)
  | Vert of t * t (* Vertical concatenation *)
  | Prepend of string * t
  | Append of t * string
  | Ind of int * t (* Indented *)

let (>>>) s d =
  match d with
  | Lit s1 -> Lit (s ^ s1)
  | _ -> Prepend (s, d)

let (<<<) d s =
  match d with
  | Lit s1 -> Lit (s1 ^ s)
  | _ -> Append (d, s)

let (+++) d1 d2 =
  match d1, d2 with
  | d, Empty | Empty, d -> d
  | Lit s1, Lit s2 -> Lit (s1 ^ s2)
  | _ -> Horz (d1, d2)

let (---) d1 d2 =
  match d1, d2 with
  | d, Empty | Empty, d -> d
  | _ -> Vert (d1, d2)

let horzs (ds: t list): t = List.fold_right (+++) ds Empty
let verts (ds: t list): t = List.fold_right (---) ds Empty

let sep_by (s: string): t list -> t list =
  let rec go = function
    | [] -> []
    | [d] -> [d]
    | d :: ds -> Append (d, s) :: go ds
  in
  go

let rec single_line: t -> bool = function
  | Empty | Lit _ -> true
  | Horz (d1, d2) -> single_line d1 && single_line d2
  | Vert _ -> false
  | Prepend (_, d) | Append (d, _) | Ind (_, d) -> single_line d

let par (d: t): t = Prepend ("(", Append (d, ")"))
let vpar (d: t): t = verts [Lit "("; d; Lit ")"]
let brac (d: t): t = Prepend ("{", Append (d, "}"))
let vbrac (d: t): t = verts [Lit "{"; d; Lit "}"]

let horz_app xs ys: string list =
  let open List in
  let rows = max (length xs) (length ys) in
  let cols = fold_left max 0 (map String.length xs) in
  let xs =
    if length xs >= rows then xs else
    xs @ init (rows - length xs) (fun _ -> String.make cols ' ')
  in
  let ys =
    if length ys >= rows then ys else
    ys @ init (rows - length ys) (fun _ -> "")
  in
  map (fun (x, y) -> x ^ y) (combine xs ys)

let rec lines: t -> string list = function
  | Empty -> []
  | Lit s -> [s]
  | Horz (d1, d2) -> horz_app (lines d1) (lines d2)
  | Vert (d1, d2) -> lines d1 @ lines d2
  | Prepend (prefix, d) ->
      (match lines d with
       | [] -> [prefix]
       | s :: ss -> (prefix ^ s) :: ss)
  | Append (d, suffix) ->
      let rec go = function
        | [] -> [suffix]
        | [s] -> [s ^ suffix]
        | s :: ss -> s :: go ss
      in
      go (lines d)
  | Ind (n, d) -> List.map ((^) (String.make n ' ')) (lines d)

let render (d: t): string = String.concat "\n" (lines d)
