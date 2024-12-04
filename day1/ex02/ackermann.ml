let rec ackermann m n =
  if m < 0 || n < 0 then
    (-1)
  else (
    match m with
    | 0 -> (n + 1)
    | _ ->
      match n with
      | 0 -> ackermann (m - 1) 1
      | _ -> ackermann (m - 1) (ackermann m (n - 1))
  )
