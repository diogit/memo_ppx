let memo = "Memoizes any function"

let %memo rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

let %memo rec lucas n b1 b2=
    match n with
    | 0 -> b1
    | 1 -> b2
    | n -> lucas (n - 1) b1 b2 + lucas (n - 2) b1 b2

let %memo rec fact n = if n = 1 then 1 else n * fact(n - 1)

(* let rec p = function [] -> () | x::xs -> (Format.printf "%i;" x); (p xs)

let rec pk l =
    let keepl = l in
    let rec keepargs = function 
    | [] -> p keepl
    | x::xs -> (Format.printf "%i;" x); (keepargs xs)
    in keepargs l

pk [1;2;3] *)