let rec ft_power a b =
  if a = 0 then
    0
  else if b = 0 then
    1
  else
    a * (ft_power a (b - 1))
