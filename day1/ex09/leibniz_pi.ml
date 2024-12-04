let leibniz_pi delta =
  let pi = 4. *. atan 1. in

  let rec pow x y res =
    match y with
    | 0 -> res
    | _ -> pow x (y - 1) (res * x)
  in

  let rec recu_pi delta pi iter res =
    if pi <= res +. delta && pi >= res -. delta
      then (
        print_float res;
        iter
      )
    else
      recu_pi delta pi (iter + 1)
      (res +. 4. *. ((float_of_int(pow (-1) iter 1)
      /. float_of_int((2 * iter) + 1))))
  in

  if delta < 0. then (-1)
  else
    recu_pi delta pi 0 0.
