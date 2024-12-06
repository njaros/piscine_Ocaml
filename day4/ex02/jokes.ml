let () =
Random.self_init ();
  let jokes = [|
    "joke1";
    "joke2";
    "joke3";
    "joke4";
    "joke5"
  |] in
  let rnd = Random.int(5) in
  print_endline (jokes.(rnd))