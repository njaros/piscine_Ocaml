(*That inhuman to create such a complex exercice in a training piscine, autor has superiority complex.
The guy wants us to master a balance sheet equation in a few hours when true chemists had to spent a lot of day to master it.
And after that we have to code it. Non sense.
Impossible without cheating because that exercice is harder and longer than some entire projects.
Let's vote with other to not do this exercice. That's sad, it makes us learn less.*)

open Atom
open Molecule

let alkanesSort (a: alkane * int) (b: alkane * int) =
  match a with
  | alA, _ -> (
    match b with
    | alB, _ -> alA#h - alB#h
  )



class virtual reaction (start: molecule list) (result: molecule list) =
object

  method virtual get_start : (molecule * int) list
  method virtual get_result : (molecule * int) list
  method virtual balance : reaction
  method virtual is_balanced : bool

end

class alkane_combustion (alkanes: alkane list) =
object
  
end