let ft_sum f a b =
  let rec sum_TR f a b res =
    if a > b then res else
      sum_TR f (a + 1) b (res +. (f a))
  in
  if a > b then nan
  else sum_TR f a b 0.
