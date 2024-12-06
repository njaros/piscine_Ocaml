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

let one_nn lst radar =
  let rec find_best lst radar best_score best_radar =
    match lst with
    | [] -> (
      match best_radar with
      | _, state -> state
      )
    | h::t -> (
      match h with
      | arrayH, _ -> (
        match radar with
        | arrayR, _ -> (
          let score = eu_dist arrayH arrayR in
          if score < best_score then
            find_best t radar score h
          else
            find_best t radar best_score best_radar
        )
      )
    )
  in
  find_best lst radar 999999999. ([||], "")