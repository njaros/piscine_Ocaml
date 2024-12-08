open Atom
open Molecule

let() =
  let a = new molecule "Carbon Monoxide" "CO" in
  let b = new carbon_dioxide in
  let c = new politicien in

  print_endline a#to_string;
  print_endline b#to_string;
  print_endline c#to_string;