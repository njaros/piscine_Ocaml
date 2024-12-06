let eu_dist a b =
  let subAndSquare x y =
    let sub = x -. y in
    sub *. sub
  in
  let allsubSquare = Array.map2 subAndSquare a b in
  sqrt (Array.fold_left ( +. ) 0. allsubSquare)

let examples_of_file path =

  let rec buildExemple lst build =
    match lst with
    | [] -> build
    | [letter] -> (
      match build with
      | array, _ -> (array, letter)
    )
    | h::t -> (
      match build with
      | array, _ -> buildExemple t ((Array.append array [|(float_of_string h)|]), "")
    )
  in

  let rec parseFile input lines =
    let line =
      try Some (input_line input) with
      | End_of_file -> None
    in
    match line with
    | None -> (
      close_in input;
      lines
    )
    | Some line ->
      parseFile input lines@[(buildExemple (String.split_on_char ',' line) ([||], ""))]
  in

  let input =
    try Some (open_in path) with
    | Sys_error _ -> None
  in
  match input with
  | None -> []
  | Some a -> List.rev (parseFile a []) (*no choice to revert the list here, for an unknown reason, it's impossible to build the list in right order with parseFile function*)

let k_nn lst n radar =

  let compare a b =
    match a with
    | scoreA, _ -> (
      match b with
      | scoreB, _ -> (
        if scoreA < scoreB then -1
        else 1
      )
    )
  in

  let compareState a b =
    match a with
    | _, nbA -> (
      match b with
      | _, nbB -> nbA - nbB
    )
  in

  let distMapping radarA =
    match radarA with
    | arrayA, stateA -> (
      match radar with
      | arrayB, _ -> ((eu_dist arrayA arrayB), stateA)
    )
  in

  let rec insertState state lst =
    match lst with
    | [] -> [(state, 1)]
    | h::t -> (
      match h with
      | st, nb -> (
        if st == state then (
          (st, (nb + 1))::t
        ) else
          h::(insertState state t)
      )
    )
  in

  let rec insertN n lst stateLst =
    match n with
    | 0 -> stateLst
    | _ -> (
      match lst with
      | [] -> stateLst
      | h::t -> (
        match h with
        | _, state ->
          insertN (n - 1) t (insertState state stateLst)
      )
    )
  in

  let distLst = List.fast_sort compare (List.map distMapping lst) in
  let eltsDist = List.fast_sort compareState (insertN n distLst []) in
  match (List.hd eltsDist) with
  | st, _ -> st
  