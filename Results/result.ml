let fib =
  let cache = Hashtbl.create 17  in
  let rec fib n =
    try Hashtbl.find cache n
    with
    | Not_found  ->
        let res = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2))  in
        (Hashtbl.add cache n res; res)
     in
  fun x  -> fib x 
