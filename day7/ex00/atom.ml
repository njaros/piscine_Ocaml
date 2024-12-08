class atom name symbol atomic_number =
object (self)

  method name: string = name
  method symbol: string = symbol
  method atomic_number: int = atomic_number

  method to_string = "atom: " ^ name ^ " | symbol: " ^ symbol ^ " | atomic_number: " ^ (string_of_int atomic_number)
  method equals (other: atom) = (atomic_number == other#atomic_number) && (name == other#name) && (symbol == other#symbol)

end

class hydrogen =
object
  inherit atom "Hydrogen" "H" 1 
end

class carbon =
object
  inherit atom "Carbon" "C" 6
end

class oxygen =
object
  inherit atom "Oxygen" "O" 8
end

class pipi =
object
  inherit atom "Pipi" "Pi" 701
end

class perlinpinpin =
object
  inherit atom "Perlinpinpin" "Pe" 702
end

class redstone =
object
  inherit atom "Redstone" "Re" 1000
end