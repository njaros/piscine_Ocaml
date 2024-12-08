open People
open Doctor
open Dalek

let () =
  Random.self_init ();
  let p = new people "random connard" in
  let d = new doctor "Maboul" 37 p in
  let dalek = new dalek in
  print_endline d#to_string;
  d#travel_in_time 1220 1233;
  print_endline d#to_string;
  d#travel_in_time 1233 0;
  print_endline d#to_string;
  d#travel_in_time 0 1300;
  print_endline d#to_string;
  dalek#talk;
  print_endline dalek#to_string;
  dalek#exterminate p;
  print_endline dalek#to_string;
  dalek#talk;
  dalek#die
