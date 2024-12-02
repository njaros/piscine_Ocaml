let rec recu_rev str i =
  match i with
  | 0 -> print_char '\n'
  | _ ->
    print_char (String.get str (i - 1));
    recu_rev str (i - 1)

let ft_print_rev str =
  let len = String.length str in
  recu_rev str len
