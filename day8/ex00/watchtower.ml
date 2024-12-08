module Watchtower = struct
  type hour = int

  let zero () = 0
  let add (h1: hour) (h2: hour) = (h1 + h2) mod 12
  let sub (h1: hour) (h2: hour) =
    let dif = (h1 - h2) mod 12 in
    if dif < 0 then 12 + dif
    else dif
end

let print_int_endline a =
  print_int a;
  print_char '\n'

let () =
  let h = Watchtower.zero () in
  print_int_endline h;
  print_int_endline (Watchtower.add 13 5);
  print_int_endline (Watchtower.sub 5 100);