open Army
open Dalek
open People
open Doctor

class galifrey (daleks: dalek army) (peoples: people army) (doctors: doctor army) =
object (self)

  val mutable _dalek_members = daleks
  val mutable _people_members = peoples
  val mutable _doctor_members = doctors

  method do_time_war =
    let rec newTurn (a: dalek army) (b: people army) (c: doctor army) =
      match (a#isDeadsaChacal, b#isDeadsaChacal, c#isDeadsaChacal) with
      | true, true, true -> print_endline "Everybody dies, gg"
      | true, true, false -> print_endline "Only doctors survives"
      | true, false, true -> print_endline "Only peoples survives"
      | false, true, true -> print_endline "Daleks killed everyone, rip"
      | true, false, false -> print_endline "Daleks exterminated"
      | false, false, true -> (
        b#delete;
        b#delete;
        if (not b#isDeadsaChacal) then
          a#delete;
        newTurn a b c
      )
      | false, true, false ->
        (
        a#delete;
        if (not a#isDeadsaChacal) then
          c#delete;
          c#delete;
        newTurn a b c
      )
      | false, false, false -> (
        a#delete;
        if (not a#isDeadsaChacal) then
          b#delete;
          c#delete;
        newTurn a b c
        )
    in
    newTurn _dalek_members _people_members _doctor_members
end