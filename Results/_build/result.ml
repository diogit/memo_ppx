let count = ref 0;;
let rec fib =
  let cache = Hashtbl.create 15  in
  function
  | n ->
      (match n with
       | 0|1 -> 1
       | _ ->
           (Format.printf "Hashtbl.num_bindings = %i\n" (Hashtbl.stats cache).num_bindings);
           if Hashtbl.mem cache n
           then (Format.printf "Cache hit: (%i, %i)\n" n (Hashtbl.find cache n); Hashtbl.find cache n)
           else
             (let res = (fib (n - 1)) + (fib (n - 2))  in
              (incr count);
              Hashtbl.add cache n res; res))
;;
(fib 20) 
