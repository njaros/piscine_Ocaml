module App = struct

  type project = string * string * int

  let zero () = ("", "", 0)

  let combine p1 p2 =
    let concatened =
      match p1 with
      | n1, _, _ -> (
        match p2 with
        | n2, _, _ -> n1 ^ n2
      )
    in
    let grade =
      match p1 with
      | _, _, n1 -> (
        match p2 with
        | _, _, n2 -> (n1 + n2) / 2
      )
    in
    if grade < 80 then
      (concatened, "failed", grade)
    else
      (concatened, "succeed", grade)
  
  let fail p = 
    match p with
    | a, _, _ -> (a, "failed", 0)

  let succedd p =
    match p with
    | a, _, _ -> (a, "succeed", 80)

end

let () =
  let print_proj p =
    match p with
    | a, b, c -> (
      print_endline a;
      print_endline b;
      print_string "score: ";
      print_int c;
      print_char '\n'
    ) in

  let proj1 = ("coucou", "succeed", 100) in
  let proj2 = ("couco", "succeed", 80) in
  let proj3 = ("couc", "failed", 59) in

  print_proj proj1;
  print_proj proj2;
  print_proj proj3;
  print_proj (App.combine (App.combine proj1 proj2) proj3)