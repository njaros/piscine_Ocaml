let rec hfs_f n =
  if n < 0 then (-1) else
  match n with
  | 0 -> 1
  | _ -> n - hfs_m (hfs_f (n - 1))

and hfs_m n =
  if n < 0 then (-1) else
  match n with
  | 0 -> 0
  | _ -> n - hfs_f (hfs_m (n - 1))
