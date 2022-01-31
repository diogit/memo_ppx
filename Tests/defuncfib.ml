open Format 

type kont =
  | Kid
  | KConstructor_0 of int * kont 
  | KConstructor_1 of int * kont

let rec fib n =
  let %memo rec fib_aux n kFunction =
    if n <= 1
    then apply kFunction 1
    else fib_aux (n - 1) (KConstructor_1 (n, kFunction))
  
  and apply kFunction arg =
    match kFunction with
    | Kid  -> arg
    | KConstructor_1 (a,kFunction) ->
        let fibANF_0 = arg  in
        fib_aux (n - 2) (KConstructor_0 (fibANF_0, kFunction))
    | KConstructor_0 (fibANF_0,kFunction) ->
        let fibANF_1 = arg  in apply kFunction (fibANF_1 + fibANF_0)
   in fib_aux n Kid 

let () = eprintf "height_CPS   : %d@." (fib 1_000_000)