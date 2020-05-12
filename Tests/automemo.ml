let fix_memo t =
  let memo = Hashtbl.create 15 in
  let rec g x =
    try Hashtbl.find memo x
    with Not_found ->
      let y = t g x in
      Hashtbl.add memo x y; y in
  g