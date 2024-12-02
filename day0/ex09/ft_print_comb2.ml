let ft_print_comb2 () =
  let rec recu a b init =
    if init == false then
      print_char ',';
      print_char ' ';
    if a < 10 then
      print_char '0';
    print_int a;
    print_char ' ';
    if b < 10 then
      print_char '0';
    print_int b;
    if b < 99 then (
      recu a (b + 1) false
    ) else if a < 98 then (
      recu (a + 1) (a + 2) false
    ) else (
      print_char '\n'
    )
    in
  recu 0 1 true
