(* Unwind *)
let %unwind rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)
let t_fib fib n = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2)) 

let %unwind rec fib n =
    match n with
    | 0 | 1 -> 1
    | n ->  fib (n-1) + fib (n-2)
let t_fib fib n =
  match n with | 0|1 -> 1 | n -> (fib (n - 1)) + (fib (n - 2)) 

let %unwind rec fib =
    function
    | 0 | 1 -> 1
    | n ->  fib (n-1) + fib (n-2)
let t_fib fib = function | 0|1 -> 1 | n -> (fib (n - 1)) + (fib (n - 2))

let %unwind rec lucas n =
    match n with
    | 0 -> 2
    | 1 -> 1
    | n -> lucas (n - 1) + lucas (n - 2)
let t_lucas lucas n =
  match n with | 0 -> 2 | 1 -> 1 | n -> (lucas (n - 1)) + (lucas (n - 2))

(* Auto Memoize *)
let fix_memo t =
  let memo = Hashtbl.create 15 in
  let rec g n =
    try Hashtbl.find memo n
    with Not_found ->
      let y = t g n in
      Hashtbl.add memo n y; y in
  g

let fib_memo = fix_memo t_fib