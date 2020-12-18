let %memo rec ackerman =
    fun m ->
        fun n ->
            match m, n with
            | 0, n -> n + 1
            | m, 0 -> ackerman (m - 1) 1
            | m, n -> ackerman (m - 1) (ackerman m (n - 1))

let %memo rec ackerman m n =
    match m, n with
    | 0, n -> n + 1
    | m, 0 -> ackerman (m - 1) 1
    | m, n -> ackerman (m - 1) (ackerman m (n - 1))