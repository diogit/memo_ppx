open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let newFun funName funBody =
  Vb.mk (Pat.var {txt = "t_"^funName; loc=(!default_loc)})
  (Exp.fun_ Nolabel None (Pat.var {txt = funName; loc=(!default_loc)}) funBody)

let unwind funName funBody = Str.value Nonrecursive [newFun funName funBody]
(* unwind functionName expression *)
(*
let fix_memo t =
  let memo = Hashtbl.create 15 in
  let rec g n =
    try Hashtbl.find memo n
    with Not_found ->
      let y = t g n in
      Hashtbl.add memo n y; y in
  g *)

let seq = Exp.sequence
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)}))
    [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
     (Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)});
     (Nolabel, Exp.ident {txt = Lident "y"; loc=(!default_loc)})]
  )
  (Exp.ident {txt = Lident "y"; loc=(!default_loc)})

let letyBinding = Vb.mk (Pat.var {txt = "y"; loc=(!default_loc)})
  (Exp.apply
    (Exp.ident {txt = Lident "t"; loc=(!default_loc)})
    [(Nolabel, Exp.ident {txt = Lident "g"; loc=(!default_loc)});
     (Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)})])

let matchRight = Exp.let_ Nonrecursive [letyBinding] seq  

let withCase = 
  {
    pc_lhs = Pat.construct {txt = Lident "Not_found"; loc=(!default_loc)}
  None; 
  	pc_guard = None;
  	pc_rhs = matchRight
  }

let cacheFind = Exp.apply
  (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "find"); loc=(!default_loc)}))
  [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
   (Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)})]

let tryExp = Exp.try_ cacheFind [withCase]

let gExp = Exp.fun_ Nolabel None ((Pat.var {txt = "n"; loc=(!default_loc)})) tryExp

let gVb = Vb.mk (Pat.var {txt = "g"; loc=(!default_loc)}) gExp

let g = Exp.let_ Recursive [gVb] (Exp.ident {txt = Lident "g"; loc=(!default_loc)})

let cacheCreate = Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)})
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)}))
    [(Nolabel, Exp.constant (Pconst_integer ("15", None)))]
  )

let memoExp = Exp.let_ Nonrecursive [cacheCreate] g

let fix_memoVb = Exp.fun_ Nolabel None (Pat.var {txt = "t"; loc=(!default_loc)}) memoExp

let fix_memo = Str.value Nonrecursive [Vb.mk (Pat.var {txt = "fix_memo"; loc=(!default_loc)}) fix_memoVb]

(* END evaluate expression*)
let rec str_item_mapper mapper str = 
   begin match str with
      | { pstr_desc =
          Pstr_extension (({ txt = "unwind"; loc }, pstr), _attributes); _} -> 
          begin 
            match pstr with
            | PStr [{ pstr_desc =
                    Pstr_value (Recursive,
                    [{
                      pvb_pat = {ppat_desc = Ppat_var {txt = functionName;_};_}; pvb_expr = expression;_
                      }]); _}] -> str_item_mapper mapper (unwind functionName expression)
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
          end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
      end

let unwind_mapper _argv =
  { 
    default_mapper with
    structure_item = str_item_mapper
  }
 
let () = register "unwind" unwind_mapper
