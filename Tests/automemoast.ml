automemo.ml
==>
[{pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "fix_memo"}};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "t"}},
          {pexp_desc =
            Pexp_let (Nonrecursive,
             [{pvb_pat = {ppat_desc = Ppat_var {txt = "memo"}};
               pvb_expr =
                {pexp_desc =
                  Pexp_apply
                   ({pexp_desc =
                      Pexp_ident {txt = Ldot (Lident "Hashtbl", "create")}},
                   [(Nolabel,
                     {pexp_desc = Pexp_constant (Pconst_integer ("15", None))})])}}],
             {pexp_desc =
               Pexp_let (Recursive,
                [{pvb_pat = {ppat_desc = Ppat_var {txt = "g"}};
                  pvb_expr =
                   {pexp_desc =
                     Pexp_fun (Nolabel, None,
                      {ppat_desc = Ppat_var {txt = "x"}},
                      {pexp_desc =
                        Pexp_try
                         ({pexp_desc =
                            Pexp_apply
                             ({pexp_desc =
                                Pexp_ident
                                 {txt = Ldot (Lident "Hashtbl", "find")}},
                             [(Nolabel,
                               {pexp_desc = Pexp_ident {txt = Lident "memo"}});
                              (Nolabel,
                               {pexp_desc = Pexp_ident {txt = Lident "x"}})])},
                         [{pc_lhs =
                            {ppat_desc =
                              Ppat_construct ({txt = Lident "Not_found"},
                               None)};
                           pc_guard = None;
                           pc_rhs = <----
                            {pexp_desc =
                              Pexp_let (Nonrecursive,
                               [{pvb_pat = {ppat_desc = Ppat_var {txt = "y"}};
                                 pvb_expr =
                                  {pexp_desc =
                                    Pexp_apply
                                     ({pexp_desc =
                                        Pexp_ident {txt = Lident "t"}},
                                     [(Nolabel,
                                       {pexp_desc =
                                         Pexp_ident {txt = Lident "g"}});
                                      (Nolabel,
                                       {pexp_desc =
                                         Pexp_ident {txt = Lident "x"}})])}}],
                               {pexp_desc =
                                 Pexp_sequence
                                  ({pexp_desc =
                                     Pexp_apply
                                      ({pexp_desc =
                                         Pexp_ident
                                          {txt =
                                            Ldot (Lident "Hashtbl", "add")}},
                                      [(Nolabel,
                                        {pexp_desc =
                                          Pexp_ident {txt = Lident "memo"}});
                                       (Nolabel,
                                        {pexp_desc =
                                          Pexp_ident {txt = Lident "x"}});
                                       (Nolabel,
                                        {pexp_desc =
                                          Pexp_ident {txt = Lident "y"}})])},
                                  {pexp_desc = Pexp_ident {txt = Lident "y"}})})}}])})}}],
                {pexp_desc = Pexp_ident {txt = Lident "g"}})})})}}])}]
=========
