open People

class dalek =
object (self)

  initializer print_endline self#to_string

  val _name =
    let generateName nameLen =
      let rec build n name =
        match n with
        | 0 -> name
        | _ -> build (n - 1) (name ^ string_of_int(Random.int(10)))
      in
      build nameLen ""
    in
  ("Dalek" ^ (generateName 3))

  val mutable _hp = 100

  val mutable _shield = true

  method exterminate (p: people) =
    p#die;
    _shield <- not _shield

  method to_string = _name ^ " | hp: " ^ (string_of_int _hp) ^ " | shield: " ^ (string_of_bool _shield)

  method talk =
    print_string (_name ^ ": ");
    match (Random.int 4) with
    | 0 -> print_endline "Explain! Explain!"
    | 1 -> print_endline "Exterminate! Exterminate!"
    | 2 -> print_endline "I obey!"
    | _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"

  method die =
    print_string (_name ^ ": ");
    print_endline "Emergency Temporal Shift!"

end