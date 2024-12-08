open Atom
open Molecule

let() =
  let soutane = new alkane "Soutane" 60 in
  let a = new methane in
  let b = new octane in
  let c = new ethane in

  print_endline soutane#to_string;
  print_endline a#to_string;
  print_endline b#to_string;
  print_endline c#to_string;