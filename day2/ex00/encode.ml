let encode a =
  let rec put_elem lst elem =
    match lst with
    | [] -> [(1, elem)]
    | [x] -> (
      match x with
      | (f, s) when s = elem -> [(f + 1), s]
      | (f, s) -> (f, s) :: (put_elem [] elem)
    )
    | h::t -> h :: (put_elem t elem)  
  in

  let rec recu_encode a res =
    match a with
    | [] -> res
    | h :: t -> recu_encode t (put_elem res h)
  in

  recu_encode a []
