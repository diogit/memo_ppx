
let %memo rec fib =
  function n ->
  match n with
  | 0 | 1 -> 1
  | _ -> fib (n-1) + fib (n-2)

fib 30