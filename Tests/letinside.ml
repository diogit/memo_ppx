let b x =
    let %memo c y = y + 1 in
    c x
let b x =
    let c =
        let cache = Hashtbl.create 16  in
        let rec c y =
            try Hashtbl.find cache y
            with
            | Not_found  ->
                let res = y + 1  in
                (Hashtbl.add cache y res; res)
            in
    fun x  -> c x 
    in c x