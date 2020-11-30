let %memo rec fact n = if n = 1 then 1 else n * fact(n - 1)

let %memo rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

let %memo rec lucas n b1 b2=
    match n with
    | 0 -> b1
    | 1 -> b2
    | n -> lucas (n - 1) b1 b2 + lucas (n - 2) b1 b2

let %memo rec ackerman m n=
    match m, n with
      | 0, n -> n + 1
      | m, 0 -> ackerman (m - 1) 1
      | m, n -> ackerman (m - 1) (ackerman m (n - 1))

let %memo rec hodstadterFemale n =
    if n = 0
    then 1
    else n - hodstadterMale (hodstadterFemale (n - 1))
    and hodstadterMale n =
    if n = 0
    then 0
    else n - hodstadterFemale (hodstadterMale (n - 1))

let %memo rec tak x y z =
    if y < x
    then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
    else z

let %memo rec comb n k =
    if n = 0 || k = 0 || k > n
    then 0
    else if k = 1 || k = n
    then 1
    else k * comb (n - 1) k + comb (n - 1) (k - 1)

let bell n =
    let rec calcComb k =
    if k = 0
    then 0
    else comb n k + calcComb (k - 1)
    in
    if n = 0
    then 1
    else calcComb n
