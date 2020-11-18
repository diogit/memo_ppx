let ccount = ref 0;;
let rec fib n = incr ccount; if n < 2 then 1 else fib (n-1) + fib (n-2)
let oc = open_out_gen [Open_creat; Open_append] 0o666 "Results/results_fib_50.txt"

let time f x name =
    let t = Sys.time() in
    let _ = f x in
    Printf.fprintf oc "Execution time of %s: %fs \n" name (Sys.time() -. t)


let () =
let func = fib in
let arg = 50 in
time func arg ("fib "^string_of_int arg)

let mem_used = Gc.allocated_bytes ()
let () = Printf.fprintf oc "Number of bytes allocated: %f MB\n" (mem_used/.1024.0/.1024.0)

let () = Printf.fprintf oc "Number of computations: %i\n" !ccount

let () = close_out oc