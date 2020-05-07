open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let rec expr_mapper mapper expr = 
   begin match expr with
      | { pexp_desc =
          Pexp_extension ({ txt = "memo"; loc }, pstr); _} ->
        begin match pstr with
        (* overflow *)
        | PStr [{pstr_desc =
          Pstr_value (Recursive,
           [{pvb_pat = {ppat_desc = Ppat_var ({txt = "fib"})};
             pvb_expr = {pexp_desc = functionContent}}])}] -> Exp.let_ Nonrecursive
             [Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)}) (Exp.apply Exp.ident {txt = Ldot (Lident "Hashtbl", "create")})] 
        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  end

let memo_mapper argv =
  { 
    default_mapper with
    expr = expr_mapper;
  }
 
let () = register "memo" memo_mapper
