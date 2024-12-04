let fibonacci n =
  let rec fiboTR n a b =
    match n with
    | 0 -> a
    | 1 -> b
    | _ ->
      fiboTR (n - 1) b (a + b)
    in
  if n < 0 then -1
  else fiboTR n 0 1
