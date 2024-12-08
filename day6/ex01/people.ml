class people name =
object(self)
  val _name: string = name
  val mutable _hp: int = 100

  initializer print_endline (self#to_string ^ " created")

  method to_string = _name ^ ": hp: " ^ (string_of_int _hp)

  method talk = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")

  method die = print_endline "Aaaarghh!"
end