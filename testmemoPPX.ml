open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* START memoize fibonacci *)
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

let aux = Exp.let_ Nonrecursive [elseVB] seq

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

let memoize_fibonacci functionName = Str.value Recursive 
                      [Vb.mk (Pat.var {txt = functionName; loc=(!default_loc)}) 
                      letcachein]
(* END memoize fibonacci *)

(* START evaluate expression*)
let rec evaluateExpression functionName = function
  | {pexp_desc = Pexp_function(caseList); pexp_attributes = att; pexp_loc = loc } -> 1
  | x -> 0

(* END evaluate expression*)
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
                      }]); _}] -> str_item_mapper mapper (memoize_fibonacci functionName)
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
