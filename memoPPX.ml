open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* DEBUG *)
let rec p = function [] -> Format.printf "[]" | x::xs -> (Format.printf "%s;" x); p xs

let rec constructList l =
  match l with
  | [] -> Exp.construct ({txt = Lident "[]"; loc=(!default_loc)}) None
  | x::xs -> Exp.construct ({txt = Lident "::"; loc=(!default_loc)}) (Some(Exp.tuple [Exp.ident {txt = Lident x; loc=(!default_loc)}; constructList xs]))

let applyHash args = Exp.apply (Exp.ident {txt = Lident "hash"; loc=(!default_loc)})
  [(Nolabel, constructList args)]

let isSingleArg args = if List.length args = 1 then true else false

let writeCacheArgs args =
  if isSingleArg args
  then Exp.ident {txt = Lident (List.hd args); loc=(!default_loc)}
  else applyHash args

let seq args = Exp.sequence
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)}))
    [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
     (Nolabel, writeCacheArgs args);
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
   (Nolabel, writeCacheArgs args)]

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

let cacheCreate = Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)})
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)}))
    [(Nolabel, Exp.constant (Pconst_integer ("15", None)))]
  )

let memoExp funName expression args = Exp.let_ Nonrecursive [cacheCreate]
  (if isSingleArg args
  then g funName expression args
  else hash funName expression args)

let fix_memo funName expression args = Str.value Nonrecursive [Vb.mk (Pat.var {txt = funName^""; loc=(!default_loc)}) (memoExp funName expression args)]

let rec getFuncBody functionName expr l =
  match expr with
  | {pexp_desc = pexp;_} -> 
    begin
    match pexp with
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = arg;_};_}, body) -> 
      getFuncBody functionName body (arg::l)
    | _ -> fix_memo functionName expr (List.rev l)
    end

let rec str_item_mapper mapper str = 
   begin match str with
      | { pstr_desc =
          Pstr_extension (({ txt = "memo"; loc }, pstr), _attributes); _} -> 
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
