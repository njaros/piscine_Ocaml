let crossover l1 l2 =
  let rec contained l1 elem =
    match l1 with
    | [] -> false
    | a :: b when a = elem -> true
    | a :: b -> contained b elem
  in

  let rec recu l1 l2 res =
    match l1 with
    | [] -> res
    | a :: b ->
      if (contained l2 a) then
        recu b l2 (a :: res)
      else
        recu b l2 res
  in

  recu l1 l2 []
