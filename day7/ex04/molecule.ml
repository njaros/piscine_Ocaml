open Atom

module AtomKey = struct
  type t = atom
  let compare (a: t) (b: t) =
    String.compare a#symbol b#symbol
end

module AtomMap = Map.Make(AtomKey)

class molecule name formula =
object
  method name: string = name
  method formula: string = formula

  method to_string = name ^ " | formula: " ^ formula

end

class trinitrotoluene =
object
  inherit molecule "Trinitrotoluene" "C7H5N3O6"
end

class water =
object
  inherit molecule "Water" "H2O"
  method c = 0
  method h = 2
  method o = 1
end

class carbon_dioxide =
object
  inherit molecule "Carbon dioxide" "CO2"
  method c = 1
  method h = 0
  method o = 2
end

class poudre_de_cheminette =
object
  inherit molecule "Poudre de cheminette" "C5Pe2Pi3"
end

class butane =
object
  inherit molecule "Butane" "C4H10"
end

class politicien =
object
  inherit molecule "Politicien" "Pi5000"
end

class dioxygen =
object
    inherit molecule "Dioxygene" "O2"
  method c = 0
  method h = 0  
  method o = 2
end

class alkane (name: string) (n: int) =
object (self)
  inherit molecule name ("C" ^ (string_of_int n) ^ "H" ^ (string_of_int (2 * n + 2)))
  method c = n
  method h = 2 * n + 2
  method o = 0

  method equals (other: alkane) = (self#name == other#name) && (self#formula == other#formula)
end

class methane =
object
  inherit alkane "Methane" 1
end

class ethane =
object
  inherit alkane "Ethane" 2
end

class octane =
object
  inherit alkane "Octane" 8
end
