let eu_dist a b =
  let subAndSquare x y =
    let sub = x -. y in
    sub *. sub
  in
  let allsubSquare = Array.map2 subAndSquare a b in
  sqrt (Array.fold_left ( +. ) 0. allsubSquare)
