let memo1 = "Rewrites any function into memoized fibonacci"

let %memo1 rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

let %memo1 rec lucas n =
    match n with
    | 0 -> 2
    | 1 -> 1
    | n -> lucas (n - 1) + lucas (n - 2)

let %memo1 rec fact n = if n = 1 then 1 else n * fact(n - 1)

type v = V of int * int
let %memo1 rec sumToK v =
    match v with
    | V(n, 0) -> 1
    | V(0, k) -> 0
    | V(n, k) -> sumToK (V(n-1, k)) + sumToK (V(n,k-1)) + sumToK(V(n-1,k-1))