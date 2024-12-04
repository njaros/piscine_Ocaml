let rec iter f x n =
  if n < 0 then -1
  else match n with
  | 0 -> x
  | _ -> f (iter f x (n - 1))
