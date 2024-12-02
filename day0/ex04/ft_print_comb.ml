let rec rec_nbr a b c init =
  if init == false then
    print_string ", ";
  print_int a;
  print_int b;
  print_int c;
  if c < 9 then (
    rec_nbr a b (c + 1) false
  ) else if b < 8 then (
    rec_nbr a (b + 1) (b + 2) false
  ) else if a < 7 then (
    rec_nbr (a + 1) (a + 2) (a + 3) false
  ) else
    print_string "\n"

let ft_print_comb () =
  rec_nbr 0 1 2 true
