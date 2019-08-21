exception No

(* lit = convert string to atom, nab(la) = fresh atom *)
let (lit, nab) =
  let memo = Hashtbl.create 100 in
  let i = ref 0 in
  let lit s =
    match Hashtbl.find_opt memo s with
    | Some n -> `Lit n
    | None -> let n = !i in Hashtbl.add memo s n; i := n + 1; `Lit n
  in
  let nab () = let n = !i in i := n + 1; `Lit n in
  (lit, nab)

(* compound term *)
let arr l = `Arr (Array.of_list l)

(* fresh unification variable *)
let gen () = let rec x = `Var (ref x) in x

let nop _ = ()

(* log = list of thunks that will undo destructive updates
   set_l x stores x in l's parent pointer location
     i.e. pointer graph goes (p -> l) x ==set_l x==> (p -> x) l
     l isn't destroyed, but just isolated from the old reference graph
*)
let rec unify log l r set_l set_r =
  let set log pa b =
    let a = !pa in
    log := (fun _ -> pa := a) :: !log;
    pa := b
  in
  match l, r, set_l, set_r with
  | _ when l == r -> ()
  | `Lit i, `Lit j, _, _ when i = j -> ()
  | `Arr xs, `Arr ys, _, _ when Array.(length xs = length ys) ->
      for i = 0 to Array.length xs - 1 do
        unify log xs.(i) ys.(i) (fun x -> xs.(i) <- x) (fun y -> ys.(i) <- y)
      done
  (* instantiate variables *)
  | (`Var pa as a), b, _, _ when a == !pa -> set log pa b
  | b, (`Var pa as a), _, _ when a == !pa -> set log pa b
  (* follow var -> var links (find representative) *)
  | `Var ({contents = `Var ppa} as pa), b, _, set_b
  | b, `Var ({contents = `Var ppa} as pa), set_b, _ ->
      unify log !ppa b ((:=) pa) set_b
  (* var ~ non-var with var -> term:
     - store var in non-var's parent pointer location
     - check term ~ non-var
     this allows for unification of cyclic structures *)
  | (`Var pa as va), b, _, set_b | b, (`Var pa as va), set_b, _ ->
      log := (fun _ -> set_b b) :: !log;
      set_b va;
      unify log !pa b ((:=) pa) nop
  | _ -> raise No

let ( =:= ) l r log = unify log l r nop nop

let undo log = List.iter (fun f -> f ()) !log; log := []

let all fs log = List.iter (fun f -> f log) fs

let any fs _ =
  let log = ref [] in
  let rec go = function
    | [] -> raise No
    | [f] -> f log
    | f :: fs -> try f log with No -> undo log; go fs
  in
  go fs

let exists f log = let x = gen () in f x log; x
let exists2 f log = let x, y = gen (), gen () in f x y log; x, y
let exists3 f log = let x, y, z = gen (), gen (), gen () in f x y z log; x, y, z
let run f = let log = ref [] in (log, f log)

let res () =
  let app go t =
    any
      [ (let xs = gen () in t =:= arr [lit "nil"; lit "++"; xs; lit "="; xs])
      ; (let x, xs, ys, zs = gen (), gen (), gen (), gen () in
        all
          [ t =:= arr
              [ arr [x; lit "::"; xs]; lit "++"; ys
              ; lit "="; arr [x; lit "::"; zs]
              ]
          ; go (arr [xs; lit "++"; ys; lit "="; zs])
          ])
      ]
  in
  let rec go t log = app go t log in
  let nil = lit "nil" in
  let cons h t = arr [h; lit "::"; t] in
  let pp = lit "++" in
  let eq = lit "=" in
  ( run @@ exists (fun xs -> go @@ arr
      [ cons (`Lit 98) (cons (`Lit 99) nil)
      ; pp; cons (`Lit 100) (cons (`Lit 101) nil)
      ; eq; cons (`Lit 98) xs
      ])
  , run @@ exists2 (fun x y -> all
      [ x =:= cons (`Lit 100) x
      ; y =:= cons (`Lit 100) (cons (`Lit 100) y)
      ; x =:= y
      ])
  , run @@ exists3 (fun x y z -> all
      [ x =:= cons (`Lit 100) x
      ; y =:= cons (`Lit 100) (cons z y)
      ; x =:= y
      ])
  , run @@ exists3 (fun x y z -> all
      [ x =:= cons (`Lit 100) x
      ; y =:= cons (`Lit 100) z
      ; z =:= cons (`Lit 100) y
      ; x =:= y
      ])
  )
