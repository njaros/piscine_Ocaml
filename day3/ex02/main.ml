open Card

let () =
  let k = Color.Spade in
  print_endline (Color.toStringVerbose k);
  let card = (Value.Jack, Color.Diamond) in
  let card2 = (Value.T9, Color.Heart) in
  let card3 = (Value.T9, Color.Club) in
  let deck = [
    card;
    card2;
    card3;
    (Value.Jack, Color.Club);
    (Value.King, Color.Diamond);
    (Value.T10, Color.Heart);
    (Value.T4, Color.Spade)
  ] in
  print_endline (toString card);
  print_endline (toStringVerbose card);
  print_endline (toString card2);
  print_endline (toStringVerbose card2);
  print_int (compare card card2);
  print_endline "";
  print_int (compare card2 card);
  print_endline "";
  print_int (compare card card);
  print_endline "";
  print_endline (toStringVerbose (min card2 card));
  print_endline (toStringVerbose (min card card2));
  print_endline (toStringVerbose (min card3 card2));
  print_endline (toStringVerbose (min card2 card3));
  print_endline (toStringVerbose (max card2 card));
  print_endline (toStringVerbose (max card card2));
  print_endline (toStringVerbose (max card3 card2));
  print_endline (toStringVerbose (max card2 card3));
  print_endline (toStringVerbose (best deck));