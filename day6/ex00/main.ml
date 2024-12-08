open People

let () =
  let a = new people "pouet" in
  print_endline (a#to_string);
  a#talk;
  a#die;