type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase
}
type helix = nucleotide list
type rna = nucleobase list
type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val
type protein = aminoacid list

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
  | _ ->
    {
      phosphate = "phosphate";
      deoxyribose = "deoxyribose";
      nucleobase = None
    }


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
    | _ -> (generate_nucleotide '_') :: (complementary_helix t)
  )

let rec generate_rna helix =
  match helix with
  | [] -> []
  | h::t -> (
    match h.nucleobase with
    | A -> U :: (generate_rna t)
    | T -> A :: (generate_rna t)
    | C -> G :: (generate_rna t)
    | G -> C :: (generate_rna t)
    | _ -> generate_rna t
  )

let generate_base_triplets rna =
  let rec recu_triplet rna a b lst =
    match (a, b) with
    | None, _ -> (
      match rna with
      | [] -> lst
      | h::t -> recu_triplet t h b lst
      )
    | _, None -> (
      match rna with
      | [] -> lst
      | h::t -> recu_triplet t a h lst
      )
    | _, _ -> (
      match rna with
      | [] -> lst
      | h::t -> recu_triplet t None None (concat lst [(a, b, h)])
      )
  in
  recu_triplet rna None None []

let string_of_protein protein =
  let rec recu lst str =
    match lst with
    | [] -> str
    | h::t -> (
      match h with
      | Stop -> str ^ "End of transaction"
      | Ala -> str ^ "Alanine "
      | Arg -> str ^ "Arginine "
      | Asn -> str ^ "Asparagine "
      | Asp -> str ^ "Aspatique "
      | Cys -> str ^ "Cysteine "
      | Gln -> str ^ "Glutamine "
      | Glu -> str ^ "Glutamine "
      | Gly -> str ^ "Glycine "
      | His -> str ^ "Histidine "
      | Ile -> str ^ "Isoleucine "
      | Leu -> str ^ "Leucine "
      | Lys -> str ^ "Lysine "
      | Met -> str ^ "Methionine "
      | Phe -> str ^ "Phenylalanine "
      | Pro -> str ^ "Proline "
      | Ser -> str ^ "Serine "
      | Thr -> str ^ "Threonine "
      | Trp -> str ^ "Tryptophane "
      | Tyr -> str ^ "Tyrosine "
      | Val -> str ^ "Valine "
    )
  in
  (recu protein "") ^ "\n"

let decode_arn arn =
  let triplets = generate_base_triplets arn in
  let rec decode_tiplets triplets =
    match triplets with
    | [] -> []
    | h::t -> (
      match h with
      | (U, A, A) | (U, A, G) | (U, G, A) -> [Stop]
      | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala::(decode_tiplets t)
      | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg::(decode_tiplets t)
      | (A, A, C) | (A, A, U) -> Asn::(decode_tiplets t)
      | (G, A, C) | (G, A, U) -> Asp::(decode_tiplets t)
      | (U, G, C) | (U, G, U) -> Cys::(decode_tiplets t)
      | (C, A, A) | (C, A, G) -> Gln::(decode_tiplets t)
      | (G, A, A) | (G, A, G) -> Glu::(decode_tiplets t)
      | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly::(decode_tiplets t)
      | (C, A, C) | (C, A, U) -> His::(decode_tiplets t)
      | (A, U, A) | (A, U, C) | (A, U, U) -> Ile::(decode_tiplets t)
      | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu::(decode_tiplets t)
      | (A, A, A) | (A, A, G) -> Lys::(decode_tiplets t)
      | (A, U, G) -> Met::(decode_tiplets t)
      | (U, U, C) | (U, U, U) -> Phe::(decode_tiplets t)
      | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro::(decode_tiplets t)
      | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser::(decode_tiplets t)
      | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr::(decode_tiplets t)
      | (U, G, G) -> Trp::(decode_tiplets t)
      | (U, A, C) | (U, A, U) -> Tyr::(decode_tiplets t)
      | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val::(decode_tiplets t)
      | _ -> decode_tiplets t
    )
  in
  decode_tiplets triplets