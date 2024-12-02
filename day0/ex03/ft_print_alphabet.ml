let rec recu_alpha a =
  print_char (char_of_int a);
  if a = int_of_char 'z' then (
    print_char '\n';
  ) else
    recu_alpha (a + 1)

let ft_print_alphabet () =
  let start = int_of_char 'a' in
  recu_alpha start;
