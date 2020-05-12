let rec fib =
  function
  | 0 | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

let rec fact =
function
| 0 -> 1
| n -> n * fact (n - 1)

let rec memo_fact =
  let cache = Hashtbl.create 15  in
  function
  | 0 -> 1
  | n ->
      if Hashtbl.mem cache n
      then Hashtbl.find cache n
      else
        (let res = n * fact (n - 1)  in
        Hashtbl.add cache n res; res)
(* doesn't work cuz recursive call is already called before *)
let rec memo fun_ arg =
  let cache = Hashtbl.create 15  in
  if Hashtbl.mem cache arg
  then Hashtbl.find cache arg
  else let res = fun_ arg in
       Hashtbl.add cache arg res; res

      