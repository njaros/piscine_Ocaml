type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase
}

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
