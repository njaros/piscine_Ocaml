let rec recu_map fn str i =
  match i with
  | 0 -> true
  | _ ->
    fn (String.get str (i - 1)) & (recu_map fn str (i - 1))

let ft_string_all fn str =
  let len = String.length str in
  recu_map fn str len
