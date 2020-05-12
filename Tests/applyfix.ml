let t_fib g n = if n < 2 then 1 else g (n-1) + g (n-2)

let fix_memo t =
  let memo = Hashtbl.create 15 in
  let rec g x =
    try Hashtbl.find memo x
    with Not_found ->
      let y = t g x in
      Hashtbl.add memo x y; y in
  g

let fib_memo = fix_memo t_fib