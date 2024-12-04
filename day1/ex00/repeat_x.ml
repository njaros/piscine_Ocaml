let rec repeat_x nb =
  if nb < 0 then
    "Error"
  else if nb == 0 then
    ""
  else
    "x" ^ repeat_x (nb - 1)
