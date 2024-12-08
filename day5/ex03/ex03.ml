(*Ok I get it, I will no try to get the good result, that's pointless.*)

module type FIXED = sig
  type t
  val of_float : float-> t
  val of_int : int-> t
  val to_float : t-> float
  val to_int : t-> int
  val to_string : t-> string
  val add : t-> t-> t
  (*val zero : t
  val one : t
  val succ : t-> t
  val pred : t-> t
  val min : t-> t-> t
  val max : t-> t-> t
  val gth : t-> t-> bool
  val lth : t-> t-> bool 
  val gte : t-> t-> bool
  val lte : t-> t-> bool
  val eqp : t-> t-> bool (** physical equality *)
  val eqs : t-> t-> bool (** structural equality *)
  val sub : t-> t-> t
  val mul : t-> t-> t
  val div : t-> t-> t
  val foreach : t-> t-> (t-> unit)-> unit *)
end

module type FRACTIONNAL_BITS = sig
  val bits : int
end

module Make(Elt : FRACTIONNAL_BITS) : FIXED = struct
  
  type t = int * bool array

  let of_float f =
    let i = Float.to_int f in
    let d = f -. (Int.to_float i) in
    let rec recu n max dec res =
      if n == max then res else (
        let div = 1. /. (2. ** (Int.to_float (n + 1))) in
        if div < dec then
          recu (n + 1) max (dec -. div) (Array.append res [|true|])
        else
          recu (n + 1) max dec (Array.append res [|false|])
      )
    in
    (i, (recu 0 Elt.bits d [||]))

  let of_int i =
    let rec doFalseArray n =
      match n with
      | 0 -> [||]
      | _ -> Array.append [|false|] (doFalseArray (n - 1))
    in
    (i, (doFalseArray Elt.bits))

  let to_float fixed =
    let f = Float.of_int (fst fixed) in
    let rec recu n max arr fl =
      if n == max then fl else (
        if arr.(n) then
          recu (n + 1) max arr (fl +. (1. /. (2. ** (Int.to_float (n + 1)))))
        else recu (n + 1) max arr fl
      )
    in
    recu 0 Elt.bits (snd fixed) f

  let to_int fixed =
    fst fixed
  
  let to_string fixed =
    Float.to_string (to_float fixed)

  let add f1 f2 =

    let xor a b =
      (a || b) && not (a && b)
    in

    let rec addArray n a b retain res =
      if n < 0 then
        (res, retain)
      else
        addArray (n - 1) a b ((a.(n) && b.(n)) || (retain && (xor b.(n) a.(n)))) (Array.append [|(xor (xor a.(n) b.(n)) retain)|] res)
    in

    let arr = addArray (Elt.bits - 1) (snd f1) (snd f2) false [||] in
    match arr with
    | pouet, true -> (((fst f1) + (fst f2) + 1), pouet)
    | pouet, false -> (((fst f1) + (fst f2)), pouet)

end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  print_endline (Fixed8.to_string x8);
  let y8 = Fixed8.of_float 21.32 in
  print_endline (Fixed8.to_string y8);
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  (* Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f-> print_endline (Fixed4.to_string f)) *)
