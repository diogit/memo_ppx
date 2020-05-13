let %memo rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

let %memo rec lucas n =
    match n with
    | 0 -> 2
    | 1 -> 1
    | n -> lucas (n - 1) + lucas (n - 2)

let %memo rec fact n = if n = 1 then 1 else n * fact(n - 1)

(* 
let %memo rec fib =
  function 
  | 0 | 1 -> 1
  | n -> fib (n-1) + fib (n-2)
   *)