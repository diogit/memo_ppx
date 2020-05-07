memo_fib.ml
==>
[{pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "count"}};
      pvb_expr =
       {pexp_desc =
         Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "ref"}},
          [(Nolabel,
            {pexp_desc = Pexp_constant (Pconst_integer ("0", None))})])}}])};
 LET
 {pstr_desc =
   REC
   Pstr_value (Recursive,
    FIB
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "fib"}};
      pvb_expr =
       {pexp_desc =
        LET
         Pexp_let (Nonrecursive,
          CACHE
          [{pvb_pat = {ppat_desc = Ppat_var {txt = "cache"}};
            pvb_expr =
             {pexp_desc =
               Hashtbl.CREATE 5
               Pexp_apply
                ({pexp_desc =
                   Pexp_ident {txt = Ldot (Lident "Hashtbl", "create")}},
                [(Nolabel,
                  {pexp_desc = Pexp_constant (Pconst_integer ("5", None))})
                ])}}
          ],
          IN
          {pexp_desc =
            FUNCTION
            Pexp_function
             N
             [{pc_lhs = {ppat_desc = Ppat_var {txt = "n"}}; pc_guard = None;
               pc_rhs =
                {pexp_desc =
                  Pexp_sequence
                   ({pexp_desc =
                      Pexp_apply
                       ({pexp_desc = Pexp_ident {txt = Lident "incr"}},
                       [(Nolabel,
                         {pexp_desc = Pexp_ident {txt = Lident "count"}})])},
                   {pexp_desc =
                     MATCH N
                     Pexp_match ({pexp_desc = Pexp_ident {txt = Lident "n"}},
                      WITH
                      [{pc_lhs =
                         {ppat_desc =
                           Ppat_or
                            ({ppat_desc =
                               Ppat_constant (Pconst_integer ("0", None))},
                            {ppat_desc =
                              Ppat_constant (Pconst_integer ("1", None))})};
                        pc_guard = None;
                        pc_rhs =
                         {pexp_desc =
                           Pexp_constant (Pconst_integer ("1", None))}};
                       {pc_lhs = {ppat_desc = Ppat_any}; pc_guard = None;
                        pc_rhs =
                         {pexp_desc =
                           Pexp_ifthenelse
                            ({pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Hashtbl", "mem")}},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_ident {txt = Lident "cache"}});
                                 (Nolabel,
                                  {pexp_desc = Pexp_ident {txt = Lident "n"}})])},
                            {pexp_desc =
                              Pexp_apply
                               ({pexp_desc =
                                  Pexp_ident
                                   {txt = Ldot (Lident "Hashtbl", "find")}},
                               [(Nolabel,
                                 {pexp_desc =
                                   Pexp_ident {txt = Lident "cache"}});
                                (Nolabel,
                                 {pexp_desc = Pexp_ident {txt = Lident "n"}})])},
                            Some
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc = Ppat_var {txt = "res"}};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_apply
                                      ({pexp_desc =
                                         Pexp_ident {txt = Lident "+"}},
                                      [(Nolabel,
                                        {pexp_desc =
                                          Pexp_apply
                                           ({pexp_desc =
                                              Pexp_ident {txt = Lident "fib"}},
                                           [(Nolabel,
                                             {pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt = Lident "-"}},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "n"}});
                                                 (Nolabel,
                                                  {pexp_desc =
                                                    Pexp_constant
                                                     (Pconst_integer ("1",
                                                       None))})])})])});
                                       (Nolabel,
                                        {pexp_desc =
                                          Pexp_apply
                                           ({pexp_desc =
                                              Pexp_ident {txt = Lident "fib"}},
                                           [(Nolabel,
                                             {pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt = Lident "-"}},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "n"}});
                                                 (Nolabel,
                                                  {pexp_desc =
                                                    Pexp_constant
                                                     (Pconst_integer ("2",
                                                       None))})])})])})])}}],
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
                                           Pexp_ident {txt = Lident "cache"}});
                                        (Nolabel,
                                         {pexp_desc =
                                           Pexp_ident {txt = Lident "n"}});
                                        (Nolabel,
                                         {pexp_desc =
                                           Pexp_ident {txt = Lident "res"}})])},
                                   {pexp_desc =
                                     Pexp_ident {txt = Lident "res"}})})})}}])})}}]})}}
    ])};
 {pstr_desc =
   Pstr_eval
    ({pexp_desc =
       Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "fib"}},
        [(Nolabel, {pexp_desc = Pexp_constant (Pconst_integer ("30", None))})])},
    ...)};
 {pstr_desc =
   Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident "count"}}, ...)}]
=========
