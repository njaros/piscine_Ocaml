let rec converges f a b =
  if b <= 0 then false
  else if a == f a then
    true
  else
    converges f (f a) (b - 1)
