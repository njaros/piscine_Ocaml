let rec ft_countdown a =
  if a <= 0 then (
    print_int 0;
    print_char '\n';
  ) else (
    print_int a;
    print_char '\n';
    ft_countdown (a - 1)
  )
