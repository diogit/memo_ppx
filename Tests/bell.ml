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