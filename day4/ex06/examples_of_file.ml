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
