let memo3 = "Memoizes any function"

let %memo3 rec fib n b = if n < 2 then b else fib (n-1) b + fib (n-2) b

let %memo3 rec lucas n =
    match n with
    | 0 -> 2
    | 1 -> 1
    | n -> lucas (n - 1) + lucas (n - 2)

let %memo3 rec fact n = if n = 1 then 1 else n * fact(n - 1)

(* Consider n dimensional arrays of integers whos absolute values sum to k. Let V(n,k) be the number of possible unique arrays for a given n, k. *)
type v = V of int * int
let %memo3 rec sumToK v =
    match v with
    | V(n, 0) -> 1
    | V(0, k) -> 0
    | V(n, k) -> sumToK (V(n-1, k)) + sumToK (V(n,k-1)) + sumToK(V(n-1,k-1))