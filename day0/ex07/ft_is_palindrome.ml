let rec recu_pal str i_first i_last =
  if i_first >= (i_last - 1) then (
    true
  ) else (
    (String.get str i_first) == (String.get str (i_last - 1))
    & recu_pal str (i_first + 1) (i_last - 1)
  )

let ft_is_palindrome str =
  let len = String.length str in
  recu_pal str 0 len