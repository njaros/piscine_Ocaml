open Deck

let rec reader lst =
  match lst with
  | [] -> ()
  | h::t ->
    print_endline h;
    reader t

let () =
  let d = newDeck () in
  reader (toStringList d);
  reader (toStringListVerbose d);

  let a, b = drawCard d in
  print_endline (Card.toStringVerbose a);
  reader (toStringList b);

  let h, i = drawCard [] in
  print_endline (Card.toStringVerbose h);
  reader (toStringList i);
