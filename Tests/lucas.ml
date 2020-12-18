let %memo rec lucas n b1 b2 =
    match n with
    | 0 -> b1
    | 1 -> b2
    | n -> lucas (n - 1) b1 b2 + lucas (n - 2) b1 b2