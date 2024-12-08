open People
open Doctor
open Dalek
open Army

let () =
  Random.self_init ();
  let a = new army [new dalek] in
  a#delete;
  a#add (new dalek);
  a#add (new dalek);
  a#add (new dalek);
  a#add (new dalek);
  a#delete;
  a#delete;
  a#delete;
  a#delete;
  a#delete;
  a#delete