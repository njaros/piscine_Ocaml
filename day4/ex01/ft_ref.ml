(* exercice undoable without chatgpt, because nobody does that, subject must help us to do this,
and the bind definition in incomprehensible I guess it should be :
"bind function apply a function to the a value from a reference to create an another
reference with the value transformed by the function." *)

type 'a ft_ref = { mutable contents: 'a }

let return a =
  { contents = a}

let get a =
  a.contents

let set a b =
  a.contents <- b

let bind a fn =
  fn a.contents

let fn a =
  if (a > 9) then
    return 99
  else
    return 11

let () =
  let a = return 5 in
  print_int (get a);
  print_endline "";
  set a 12;
  print_int (get a);
  let b = bind a fn in
  print_endline "";
  print_int (get b)