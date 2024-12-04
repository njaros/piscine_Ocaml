let rec repeat_string ?(str="x") nb =
  if nb < 0 then
    "Error"
  else if nb == 0 then
    ""
  else
    str ^ (repeat_string ~str:str (nb - 1))
