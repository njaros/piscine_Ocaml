let () =
  let all = Color.all in
  let rec fn cards =
    match cards with
    | [] -> ()
    | h::t ->
      print_endline (Color.toString h);
      print_endline (Color.toStringVerbose h);
      fn t
  in
  fn all
