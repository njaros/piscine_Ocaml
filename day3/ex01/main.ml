let () =
let all = Value.all in
let rec fn all =
  match all with
  | [] -> ()
  | h::t ->
    print_int (Value.toInt h);
    print_endline "";
    print_endline (Value.toString h);
    print_endline (Value.toStringVerbose h);
    if (Value.toInt h) != 13 then (
      print_string "next is ";
      print_endline (Value.toStringVerbose (Value.next h));
      );
    if (Value.toInt h) != 1 then (
      print_string "previous is ";
      print_endline (Value.toStringVerbose (Value.previous h));
      );
    fn t
  in
  fn all