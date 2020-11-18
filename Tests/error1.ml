let memo = "Memoizes any function"

let %memo rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

let %memo rec fib = function
    n when n < 2 -> 1
    | n when n >= 2 -> fib (n-1) + fib (n-2)
    | _ -> -1