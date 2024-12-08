open People
open Doctor
open Dalek
open Army
open Galifrey

let () =
  Random.self_init ();
  let rec dalekList n lst =
    match n with
    | 0 -> lst
    | _ -> dalekList (n - 1) ((new dalek) :: lst)
  in

  let rec peopleList n lst =
    match n with
    | 0 -> lst
    | _ -> peopleList (n - 1) ((new people "tomateGuy") :: lst)
  in

  let rec doctorList n lst =
    match n with
    | 0 -> lst
    | _ -> doctorList (n - 1) ((new doctor "House" 22 (new people "sbirudeskasuru")) :: lst)
  in

  let dalekArmy = new army (dalekList (Random.int(25)) []) in
  let peopleArmy = new army (peopleList (Random.int(25)) []) in
  let doctorArmy = new army (doctorList (Random.int(25)) []) in

  let war = new galifrey dalekArmy peopleArmy doctorArmy in
  war#do_time_war