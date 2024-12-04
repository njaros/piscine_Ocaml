type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase
}
type helix = nucleotide list

let rec take_last lst =
  match lst with
  | [] -> failwith "empty list"
  | [x] -> x
  | h::t -> take_last t

let rec drop_last lst =
  match lst with
  | [] -> []
  | [x] -> []
  | h::t -> h :: (drop_last t)

let rec concat l1 l2 =
  match l1 with
  | [] -> l2
  | _ -> concat (drop_last l1) ((take_last l1)::l2)

let generate_nucleotide c =
  match c with
  | 'A' ->
    {
      phosphate = "phosphate";
      deoxyribose = "deoxyribose";
      nucleobase = A
    }
  | 'T' ->
    {
      phosphate = "phosphate";
      deoxyribose = "deoxyribose";
      nucleobase = T
    }
  | 'C' ->
    {
      phosphate = "phosphate";
      deoxyribose = "deoxyribose";
      nucleobase = C
    }
  | 'G' ->
    {
      phosphate = "phosphate";
      deoxyribose = "deoxyribose";
      nucleobase = G
    }
  | _ -> failwith "this nucleobase doesn't exists"


let generate_helix n =
  if n <= 0 then []
  else (

    let take_random_letter () =
      let rand = Random.int(3) in
      match rand with
      | 0 -> 'A'
      | 1 -> 'T'
      | 2 -> 'C'
      | 3 -> 'G'
      | _ -> failwith ""
    in

    let rec recu n helix =
      match n with
      | 0 -> helix
      | _ -> recu (n - 1) ((generate_nucleotide (take_random_letter ()))::helix)
    in

    recu n []
  )

let rec complementary_helix helix =
  match helix with
  | [] -> []
  | h::t -> (
    match h.nucleobase with
    | A -> (generate_nucleotide 'T') :: (complementary_helix t)
    | T -> (generate_nucleotide 'A') :: (complementary_helix t)
    | C -> (generate_nucleotide 'G') :: (complementary_helix t)
    | G -> (generate_nucleotide 'C') :: (complementary_helix t)
    | None -> complementary_helix t
  )