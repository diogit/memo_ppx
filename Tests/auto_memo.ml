let rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2);;
let t_fib g n = if n < 2 then 1 else g (n-1) + g (n-2);;

(* let fix t = let rec g x = t g x in g *)

(* let fib = fix t_fib *)

let fix_memo t =
  let memo = Hashtbl.create 15 in
  let rec g x =
    try Hashtbl.find memo x
    with Not_found ->
      let y = t g x in
      Hashtbl.add memo x y; y in
  g

let fib_memo = fix_memo t_fib

let t_fib g n = if n < 2 then 1 else g (n-1) + g (n-2);;

(* let fix t = let rec g x = t g x in g *)

(* let fib = fix t_fib *)

let fix_memo =
  let memo = Hashtbl.create 15 in
  let rec g n =
    try Hashtbl.find memo n
    with Not_found ->
      let y = if n < 2 then 1 else g (n-1) + g (n-2) in
      Hashtbl.add memo n y; y in
  g

let fib_memo = fix_memo t_fib

(******************************************************************************)

let rec lucas n =
    match n with
    | 0 -> 2
    | 1 -> 1
    | n -> lucas (n - 1) + lucas (n - 2)

let t_lucas lucas n =
    match n with
    | 0 -> 2
    | 1 -> 1
    | n -> lucas (n - 1) + lucas (n - 2)

let lucas_memo = fix_memo t_lucas