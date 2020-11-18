let ccount = ref 0 
let fib =
  let cache = Hashtbl.create 15  in
  let rec hash = function | [] -> "" | x::xs -> (string_of_int x) ^ (hash xs)
     in
  let rec fib n =
    try Hashtbl.find cache (hash [n])
    with
    | Not_found  ->
        let y =
          incr ccount; if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2))  in
        (Hashtbl.add cache (hash [n]) y; y)
     in
  fib

let n = 1_000_000
let oc = open_out_gen [Open_creat; Open_append] 0o666 ("Results/results_memo_fib_"^string_of_int n^".txt")

let time f x name =
  let t = Sys.time ()  in
  let _ = f x  in
  Printf.fprintf oc "Execution time of %s: %fs\n" name ((Sys.time ()) -. t) 

let () =
  let func = fib  in
  let arg = n  in
  time func arg ("fib " ^ (string_of_int arg)) 
let mem_used = Gc.allocated_bytes () 
let () =
  Printf.fprintf oc "Number of bytes allocated: %f MB\n"
    ((mem_used /. 1024.0) /. 1024.0)
  
let () = Printf.fprintf oc "Number of computations: %i\n" (!ccount) 

let () = close_out oc
