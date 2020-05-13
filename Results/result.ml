let fib_memo =
  let cache = Hashtbl.create 15  in
  let rec fib n =
    try Hashtbl.find cache n
    with
    | Not_found  ->
        let y = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2))  in
        (Hashtbl.add cache n y; y)
     in
  fib 
let lucas_memo =
  let cache = Hashtbl.create 15  in
  let rec lucas n =
    try Hashtbl.find cache n
    with
    | Not_found  ->
        let y =
          match n with
          | 0 -> 2
          | 1 -> 1
          | n -> (lucas (n - 1)) + (lucas (n - 2))  in
        (Hashtbl.add cache n y; y)
     in
  lucas 
let fact_memo =
  let cache = Hashtbl.create 15  in
  let rec fact n =
    try Hashtbl.find cache n
    with
    | Not_found  ->
        let y = if n = 1 then 1 else n * (fact (n - 1))  in
        (Hashtbl.add cache n y; y)
     in
  fact 
