fib.ml
==>
[{pstr_desc =
   Pstr_extension
    (({txt = "memo"},
      PStr
       [{pstr_desc =
          Pstr_value (Recursive,
           [{pvb_pat = {ppat_desc = Ppat_var {txt = "fib"}};
             pvb_expr =
              {pexp_desc =
                Pexp_function
                 [{pc_lhs =
                    {ppat_desc =
                      Ppat_or
                       ({ppat_desc =
                          Ppat_constant (Pconst_integer ("0", None))},
                       {ppat_desc =
                         Ppat_constant (Pconst_integer ("1", None))})};
                   pc_guard = None;
                   pc_rhs =
                    {pexp_desc = Pexp_constant (Pconst_integer ("1", None))}};
                  {pc_lhs = {ppat_desc = Ppat_var {txt = "n"}};
                   pc_guard = None;
                   pc_rhs =
                    {pexp_desc =
                      Pexp_apply
                       ({pexp_desc = Pexp_ident {txt = Lident "+"}},
                       [(Nolabel,
                         {pexp_desc =
                           Pexp_apply
                            ({pexp_desc = Pexp_ident {txt = Lident "fib"}},
                            [(Nolabel,
                              {pexp_desc =
                                Pexp_apply
                                 ({pexp_desc = Pexp_ident {txt = Lident "-"}},
                                 [(Nolabel,
                                   {pexp_desc = Pexp_ident {txt = Lident "n"}});
                                  (Nolabel,
                                   {pexp_desc =
                                     Pexp_constant
                                      (Pconst_integer ("1", None))})])})])});
                        (Nolabel,
                         {pexp_desc =
                           Pexp_apply
                            ({pexp_desc = Pexp_ident {txt = Lident "fib"}},
                            [(Nolabel,
                              {pexp_desc =
                                Pexp_apply
                                 ({pexp_desc = Pexp_ident {txt = Lident "-"}},
                                 [(Nolabel,
                                   {pexp_desc = Pexp_ident {txt = Lident "n"}});
                                  (Nolabel,
                                   {pexp_desc =
                                     Pexp_constant
                                      (Pconst_integer ("2", None))})])})])})])}}]}}])}]),
    ...)}]
=========
