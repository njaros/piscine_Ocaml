module Color = struct
  
  type t = Spade | Heart | Diamond | Club
  
  let all =
    [Spade; Heart; Diamond; Club]
    
  let toString card =
    match card with
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"
      
  let toStringVerbose card =
    match card with
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"
        
end

module Value = struct

  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  let all = [T2;T3;T4;T5;T6;T7;T8;T9;T10;Jack;Queen;King;As]

  let toInt value =
    match value with
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13

  let toString value =
    match value with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose value =
    match value with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"

  let next value =
    match value with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "Nothing is higher than As."

  let previous value =
    match value with
    | T2 -> invalid_arg "Nothing is lower than T2."
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King

end

type t = Value.t * Color.t

let newCard (a: Value.t) (b: Color.t) =
  (a, b)

let allSpades () =
  let rec recu all lst =
    match all with
    | [] -> lst
    | h::t -> recu t ((newCard h Color.Spade)::lst)
  in
  recu Value.all []

let allHearts () =
  let rec recu all lst =
    match all with
    | [] -> lst
    | h::t -> recu t ((newCard h Color.Heart)::lst)
  in
  recu Value.all []

let allDiamonds () =
  let rec recu all lst =
    match all with
    | [] -> lst
    | h::t -> recu t ((newCard h Color.Diamond)::lst)
  in
  recu Value.all []

let allClubs () =
  let rec recu all lst =
    match all with
    | [] -> lst
    | h::t -> recu t ((newCard h Color.Club)::lst)
  in
  recu Value.all []

let all () =
  (allSpades ()) @ (allClubs ()) @ (allDiamonds ()) @ (allHearts ())

let getValue (card: t) =
  match card with
  | (a, b) -> a

let getColor (card: t) =
  match card with
  | (a, b) -> b

let toString card =
  match card with
  | (value, color) ->
      (Value.toString value) ^ (Color.toString color)

let toStringVerbose card =
  match card with
  | (value, color) ->
      "Card(" ^ (Value.toStringVerbose value) ^ ", " ^ (Color.toStringVerbose color) ^ ")"

let compare a b =
  match a with
  | (a_val, _) -> (
    match b with
    | (b_val, _) ->
      (Value.toInt a_val) - (Value.toInt b_val)
  )

let max a b =
  if (compare a b) < 0 then b
  else a

let min a b =
  if (compare a b) <= 0 then a
  else b

let best lst =
  match lst with
  | [] -> invalid_arg "no card"
  | _ -> List.fold_left max (Value.T2, Color.Spade) lst

let isOf card (color: Color.t) =
  if (Color.toString (getColor card)) == (Color.toString color) then true
  else false

let isSpade card =
  isOf card Color.Spade

let isHeart card =
  isOf card Color.Heart

let isDiamond card =
  isOf card Color.Diamond

let isClub card =
  isOf card Color.Club
