Tests/constant.ml
==>
[{pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "b"}};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "x"}},
          {pexp_desc =
            Pexp_extension
             ({txt = "memo"},
              PStr
               [{pstr_desc =
                  Pstr_eval
                   ({pexp_desc =
                      Pexp_let (Nonrecursive,
                       [{pvb_pat = {ppat_desc = Ppat_var {txt = "c"}};
                         pvb_expr =
                          {pexp_desc =
                            Pexp_fun (Nolabel, None,
                             {ppat_desc = Ppat_var {txt = "y"}},
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("3", None))})}}],
                       {pexp_desc = Pexp_ident {txt = Lident "c"}})},
                   ...)}])})}}])}]
=========
