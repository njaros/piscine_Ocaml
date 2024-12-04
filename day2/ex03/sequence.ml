let sequence n =
  if n <= 0 then ""
  else (

    let rec take_last lst =
      match lst with
      | [] -> failwith "empty list"
      | [x] -> x
      | h::t -> take_last t
    in

    let rec drop_last lst =
      match lst with
      | [] -> []
      | [x] -> []
      | h::t -> h :: (drop_last t)
    in

    let rec concat l1 l2 =
      match l1 with
      | [] -> l2
      | _ -> concat (drop_last l1) ((take_last l1)::l2)
    in

    let rec build_new_lst lst nb occ new_lst =
      match lst with
      | [] -> concat new_lst (occ::[nb])
      | h::t ->
        if h == nb then
          build_new_lst t nb (occ + 1) new_lst
        else if nb != 0 then
          build_new_lst t h 1 (concat new_lst (occ::[nb]))
        else
          build_new_lst t h 1 new_lst
    in

    let rec to_string lst str =
      match lst with
      | [] -> str
      | h :: t -> (
        match h with
        | 1 -> to_string t (str ^ "1")
        | 2 -> to_string t (str ^ "2")
        | 3 -> to_string t (str ^ "3")
        | _ -> failwith "implementation error"
      )
    in

    let rec recu n lst =
      match n with
      | 1 -> to_string lst ""
      | _ -> recu (n - 1) (build_new_lst lst 0 0 [])
    in

    recu n [1]
  )