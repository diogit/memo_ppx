open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(********************************************************************************)
                                    (* MEMO 1 *)
(********************************************************************************)
let hashtblCreate = Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)})
let argsCreate = [(Nolabel, Exp.constant (Pconst_integer ("15", None)))]

let apply = Exp.apply hashtblCreate argsCreate

let cache = Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)}) apply

let fibPat = Pat.var {txt = "fib"; loc=(!default_loc)}

let matchExp = Exp.ident {txt = Lident "n"; loc=(!default_loc)}

let leftPat = Pat.or_ (Pat.constant (Pconst_integer ("0", None))) (Pat.constant (Pconst_integer ("1", None)))

let case1 = {
    pc_lhs = leftPat; 
  	pc_guard = None;
  	pc_rhs = Exp.constant (Pconst_integer ("1", None))
}

let hashtblMem = Exp.ident {txt = Ldot (Lident "Hashtbl", "mem"); loc=(!default_loc)}

let argsMem = [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
          (Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)})]
let hashtblFind = Exp.ident ({ txt = Ldot (Lident "Hashtbl", "find"); loc=(!default_loc)})

let argsFind = [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
          (Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)})]

let plus = Exp.ident {txt = Lident "+"; loc=(!default_loc)}

let minus = Exp.ident {txt = Lident "-"; loc=(!default_loc)}

let argsMinus = [(Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)});
                 (Nolabel, Exp.constant (Pconst_integer ("1", None)))]

let fib = Exp.ident {txt = Lident "fib"; loc=(!default_loc)}

let argsFib1 = [(Nolabel, Exp.apply minus argsMinus)]

let argPlus1 = Exp.apply fib argsFib1

let argsMinus = [(Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)});
                 (Nolabel, Exp.constant (Pconst_integer ("2", None)))]

let argsFib2 = [(Nolabel, Exp.apply minus argsMinus)]

let argPlus2 = Exp.apply fib argsFib2

let argsPlus = [(Nolabel, argPlus1); (Nolabel, argPlus2)]

let elseVB = Vb.mk (Pat.var {txt = "res"; loc=(!default_loc)}) (Exp.apply plus argsPlus)


let res = Exp.ident {txt = Lident "res"; loc=(!default_loc)}

let hashtblAdd = Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)})


let argsAdd = [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
               (Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)});
               (Nolabel, res)]

let seq = Exp.sequence (Exp.apply hashtblAdd argsAdd) res

let rightExp = Exp.ifthenelse (Exp.apply hashtblMem argsMem) (Exp.apply hashtblFind argsFind) (Some(Exp.let_ Nonrecursive [elseVB] seq))

let case2 = {
    pc_lhs = Pat.any (); 
  	pc_guard = None;
  	pc_rhs = rightExp
}
let matchn = Exp.match_ matchExp [case1; case2]

let funCase = {
    pc_lhs = Pat.var {txt = "n"; loc=(!default_loc)}; 
  	pc_guard = None;
  	pc_rhs = matchn
}

let inn = Exp.function_ [funCase]

let letcachein = Exp.let_ Nonrecursive [cache] inn

let fibonacci = Str.value Recursive [Vb.mk fibPat letcachein]

let memoize_fibonacci = Str.value Recursive 
                      [Vb.mk (Pat.var {txt = "fib"; loc=(!default_loc)}) 
                      letcachein]
(********************************************************************************)
                                    (* MEMO 1 *)
(********************************************************************************)
(********************************************************************************)
                                    (* MEMO 3 *)
(********************************************************************************)
let rec constructList l =
  match l with
  | [] -> Exp.construct ({txt = Lident "[]"; loc=(!default_loc)}) None
  | x::xs -> Exp.construct ({txt = Lident "::"; loc=(!default_loc)}) (Some(Exp.tuple [Exp.ident {txt = Lident x; loc=(!default_loc)}; constructList xs]))

let applyHash args = Exp.apply (Exp.ident {txt = Lident "hash"; loc=(!default_loc)})
  [(Nolabel, constructList args)]

let seq args = Exp.sequence
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)}))
    [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
     (Nolabel, applyHash args);
     (Nolabel, Exp.ident {txt = Lident "y"; loc=(!default_loc)})]
  )
  (Exp.ident {txt = Lident "y"; loc=(!default_loc)})

let letyBinding expression = Vb.mk (Pat.var {txt = "y"; loc=(!default_loc)})
  (expression)

let matchRight expression args = Exp.let_ Nonrecursive [(letyBinding expression)] (seq args)  

let withCase expression args = 
  {
    pc_lhs = Pat.construct {txt = Lident "Not_found"; loc=(!default_loc)}
  None; 
  	pc_guard = None;
  	pc_rhs = (matchRight expression args)
  }

let cacheFind args = Exp.apply
  (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "find"); loc=(!default_loc)}))
  [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
   (Nolabel, applyHash args)]

let tryExp expression args = Exp.try_ (cacheFind args) [(withCase expression args)]

let rec gExp expression args =
  let keepArgs = args in
  let rec writeArgs = function
  | [] -> (tryExp expression keepArgs)
  | x::xs -> Exp.fun_ Nolabel None ((Pat.var {txt = x; loc=(!default_loc)})) (writeArgs xs)
  in writeArgs args

let g funName expression args = Exp.let_ Recursive
  [Vb.mk (Pat.var {txt = funName; loc=(!default_loc)}) (gExp expression args)] (Exp.ident {txt = Lident funName; loc=(!default_loc)})

let recRight =
  Exp.apply
    (Exp.ident {txt = Lident "^"; loc=(!default_loc)})
    [Nolabel,
      (Exp.apply
        (Exp.ident {txt = Lident "string_of_int"; loc=(!default_loc)})
        [Nolabel, Exp.ident {txt = Lident "x"; loc=(!default_loc)}]);
    (Nolabel,
      (Exp.apply (Exp.ident {txt = Lident "hash"; loc=(!default_loc)})
      [Nolabel, Exp.ident {txt = Lident "xs"; loc=(!default_loc)}]))
    ]

let hashRec =
  {
    pc_lhs = Pat.construct ({txt = Lident "::"; loc=(!default_loc)})
    (Some(Pat.tuple [
      (Pat.var {txt = "x"; loc=(!default_loc)});
      (Pat.var {txt = "xs"; loc=(!default_loc)})
    ]));
  	pc_guard = None;
  	pc_rhs = recRight
  }

let hashBase =
  {
    pc_lhs = Pat.construct {txt = Lident "[]"; loc=(!default_loc)} None; 
  	pc_guard = None;
  	pc_rhs = Exp.constant (Pconst_string ("", None))
  }

let hash funName expression args = Exp.let_ Recursive [(Vb.mk (Pat.var {txt = "hash"; loc=(!default_loc)}) (Exp.function_ [hashBase; hashRec]))] (g funName expression args)

let noHashCase args =
  {
    pc_lhs = Pat.construct {txt = Lident (List.hd args); loc=(!default_loc)} None; 
  	pc_guard = None;
  	pc_rhs = (Exp.ident {txt = Lident (List.hd args); loc=(!default_loc)})
  }

let noHash funName expression args = Exp.let_ Nonrecursive [(Vb.mk (Pat.var {txt = "hash"; loc=(!default_loc)}) (Exp.function_ [noHashCase args]))] (g funName expression args)

let cacheCreate = Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)})
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)}))
    [(Nolabel, Exp.constant (Pconst_integer ("15", None)))]
  )

let memoExp funName expression args = Exp.let_ Nonrecursive [cacheCreate]
  (if List.length args > 1 then
  (hash funName expression args)
  else (noHash funName expression args))

let fix_memo funName expression args = Str.value Nonrecursive [Vb.mk (Pat.var {txt = funName^"_memo"; loc=(!default_loc)}) (memoExp funName expression args)]

let rec getFuncBody functionName expr l =
  match expr with
  | {pexp_desc = pexp;_} -> 
    begin
    match pexp with
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = arg;_};_}, body) -> 
      getFuncBody functionName body (arg::l)
    | _ -> fix_memo functionName expr (List.rev l)
    end
(********************************************************************************)
                                    (* MEMO 3 *)
(********************************************************************************)
(********************************************************************************)
                                    (* MEMO 2 *)
(********************************************************************************)
let globalCache functionName expr =
  match expr with
  | {pexp_desc = Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = arg;_};_} ,body);_} -> 
    Str.value Recursive [Vb.mk (Pat.var {txt = functionName; loc=(!default_loc)})
      (Exp.fun_ Nolabel None (Pat.var {txt = arg; loc=(!default_loc)}) (Exp.let_ Recursive [(Vb.mk (Pat.var {txt = "hash"; loc=(!default_loc)}) (Exp.function_ [hashBase; hashRec]))]
      (tryExp body [arg])))]
  | _ -> raise (Location.Error (Location.error "Syntax error in expression mapper"))
(********************************************************************************)
                                    (* MEMO 2 *)
(********************************************************************************)
let rec str_item_mapper mapper str = 
    begin match str with
        | { pstr_desc = Pstr_extension (({ txt = "memo1"; loc }, pstr), _attributes); _} -> 
            begin 
                match pstr with
                | PStr [{ pstr_desc =
                        Pstr_value (Recursive,
                        [{
                            pvb_pat = {ppat_desc = Ppat_var {txt = _;_};_}; pvb_expr = _;_
                        }]); _}] -> str_item_mapper mapper memoize_fibonacci
                | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
            end
        | { pstr_desc = Pstr_extension (({ txt = "memo2"; loc }, pstr), _attributes); _} -> 
            begin 
                match pstr with
                | PStr [{ pstr_desc =
                        Pstr_value (Recursive,
                        [{
                            pvb_pat = {ppat_desc = Ppat_var {txt = functionName;_};_}; pvb_expr = expression;_
                        }]); _}] -> str_item_mapper mapper (globalCache functionName expression)
                | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
            end
        | { pstr_desc = Pstr_extension (({ txt = "memo3"; loc }, pstr), _attributes); _} -> 
            begin 
                match pstr with
                | PStr [{ pstr_desc =
                        Pstr_value (Recursive,
                        [{
                            pvb_pat = {ppat_desc = Ppat_var {txt = functionName;_};_}; pvb_expr = expression;_
                        }]); _}] -> str_item_mapper mapper (getFuncBody functionName expression [])
                | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
            end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
      end
let memo_mapper _argv =
  { 
    default_mapper with
    structure_item = str_item_mapper
  }
 
let () = register "memo" memo_mapper
