(* recursive function *)
let rec fib n = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2)) 

(* 1st step *)
let t_fib g n = if n < 2 then 1 else g (n-1) + g (n-2)

(* 2nd step *)
let fix_memo t =
  let memo = Hashtbl.create 15 in
  let rec g x =
    try Hashtbl.find memo x
    with Not_found ->
      let y = t g x in
      Hashtbl.add memo x y; y in
  g

(* 3rd step *)
let fib_memo = fix_memo t_fib