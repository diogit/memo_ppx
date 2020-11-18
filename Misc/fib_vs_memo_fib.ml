let count = ref 0
let rec fib =
  function n ->
  incr count;
  match n with
  | 0 | 1 -> 1
  | _ -> fib (n-1) + fib (n-2)
;;

fib 30;;
count;;

let count = ref 0;;
let rec fib =
  let cache = Hashtbl.create 5 in
  function n ->
  incr count;
  match n with
  | 0 | 1 -> 1
  | _ ->
  if Hashtbl.mem cache n
  then Hashtbl.find cache n
  else let res = fib (n-1) + fib (n-2) in
       Hashtbl.add cache n res; res
;;

fib 30;;
count;;