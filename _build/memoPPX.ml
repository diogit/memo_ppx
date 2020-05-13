open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let seq arg = Exp.sequence
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)}))
    [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
     (Nolabel, Exp.ident {txt = Lident arg; loc=(!default_loc)});
     (Nolabel, Exp.ident {txt = Lident "y"; loc=(!default_loc)})]
  )
  (Exp.ident {txt = Lident "y"; loc=(!default_loc)})

let letyBinding expression = Vb.mk (Pat.var {txt = "y"; loc=(!default_loc)})
  (expression)

let matchRight expression arg = Exp.let_ Nonrecursive [(letyBinding expression)] (seq arg)  

let withCase expression arg = 
  {
    pc_lhs = Pat.construct {txt = Lident "Not_found"; loc=(!default_loc)}
  None; 
  	pc_guard = None;
  	pc_rhs = (matchRight expression arg)
  }

let cacheFind arg = Exp.apply
  (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "find"); loc=(!default_loc)}))
  [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
   (Nolabel, Exp.ident {txt = Lident arg; loc=(!default_loc)})]

let tryExp expression arg = Exp.try_ (cacheFind arg) [(withCase expression arg)]

let gExp expression arg = Exp.fun_ Nolabel None ((Pat.var {txt = arg; loc=(!default_loc)})) (tryExp expression arg)

let gVb funName expression arg = Vb.mk (Pat.var {txt = funName; loc=(!default_loc)}) (gExp expression arg)

let g funName expression arg = Exp.let_ Recursive [gVb funName expression arg] (Exp.ident {txt = Lident funName; loc=(!default_loc)})

let cacheCreate = Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)})
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)}))
    [(Nolabel, Exp.constant (Pconst_integer ("15", None)))]
  )

let memoExp funName expression arg = Exp.let_ Nonrecursive [cacheCreate] (g funName expression arg)

let fix_memoVb funName expression arg = Exp.fun_ Nolabel None (Pat.var {txt = "t_"^funName; loc=(!default_loc)}) (memoExp funName expression arg)

let fix_memo funName expression arg = Str.value Nonrecursive [Vb.mk (Pat.var {txt = funName^"_memo"; loc=(!default_loc)}) (memoExp funName expression arg)]

let getFuncBody functionName expr =
  match expr with
  | {pexp_desc = Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = arg;_};_} ,body);} -> fix_memo functionName body arg


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
                      }]); _}] -> str_item_mapper mapper (getFuncBody functionName expression)
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
