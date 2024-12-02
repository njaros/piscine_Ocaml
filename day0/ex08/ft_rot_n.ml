let ft_rot_n n str =
  let rot n c =
    let z = int_of_char 'z' in
    let z_up = int_of_char 'Z' in
    let modulo = n mod 26 in
    let c_int = int_of_char c in
    if c >= 'a' && c <= 'z' then (
      if z - c_int < modulo then (
        char_of_int ((int_of_char 'a') + (modulo - (z - c_int)) - 1)
      ) else (
        char_of_int (c_int + modulo)
      )
    ) else if c >= 'A' && c <= 'Z' then (
      if z_up - c_int < modulo then (
        char_of_int ((int_of_char 'A') + (modulo - (z_up - c_int)) - 1)
      ) else (
        char_of_int (c_int + modulo)
      )
    ) else c in
  String.map (rot n) str
