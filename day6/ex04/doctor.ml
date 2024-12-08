open People

class doctor name age sidekick=
object (self)

  initializer print_endline (self#to_string ^ " created")

  val _name: string = name
  val mutable _age: int = age
  val mutable _sidekick: people = sidekick
  val mutable _hp: int = 100

  method to_string =
    _name ^ ": age: " ^ (string_of_int _age) ^ " | kick of the side :" ^ _sidekick#to_string ^ " | hp: " ^ (string_of_int _hp)

  method talk = print_endline "Hi! I'm the Doctor!"

  method travel_in_time start arrival =
    print_endline "    ___________";
    print_endline "    | __ | __ |";
    print_endline "    | LI | LI |";
    print_endline "    |    |    |";
    print_endline "    |    |;   |";
    print_endline "    |    |    |";
    print_endline "    |    |    |";
    print_endline "    ^^^^^^^^^^^";
    _age <- _age + (arrival - start)

  method use_sonic_screwdriver =
    print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

  method private regenerate = _hp <- 100

end