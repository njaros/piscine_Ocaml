let gray n =
  if n == 0 then ()
  else (

    let rec print_rec strs =
      match strs with
      | [] -> ()
      | a::b ->
        print_string a;
        print_char ' ';
        print_rec b;
    in

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

    let add_char_0 str = "0" ^ str in

    let add_char_1 str = "1" ^ str in

    let rec concat l1 l2 =
      match l1 with
      | [] -> l2
      | _ -> concat (drop_last l1) ((take_last l1)::l2)
    in

    let rec iter_in_list fn lst new_lst =
      match lst with
      | [] -> new_lst
      | a :: b -> iter_in_list fn b (concat new_lst [(fn a)])
    in

    let rec iter_in_list_revert fn lst new_lst =
      match lst with
      | [] -> new_lst
      | a :: b -> iter_in_list_revert fn b ((fn a) :: new_lst)
    in

    let rec mirror i n strs =
      if i == n then
        print_rec strs
      else
        mirror (i + 1) n (concat (iter_in_list add_char_0 strs []) (iter_in_list_revert add_char_1 strs []))
      in
    
    mirror 1 n ["0"; "1"];
    print_char '\n'
    )