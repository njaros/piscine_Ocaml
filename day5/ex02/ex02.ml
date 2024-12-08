module type PAIR = sig
  val pair : (int * int)
end

module type VAL = sig
  val x : int
end

module MakeFst(Elt: PAIR) : VAL = struct
  let x = fst Elt.pair
end

module MakeSnd(Elt: PAIR) : VAL = struct
  let x = snd Elt.pair
end

module Pair : PAIR = struct let pair = ( 21, 42 ) end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () =
  Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x