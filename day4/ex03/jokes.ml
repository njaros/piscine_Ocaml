let rec deli i lst new_lst =
  match lst with
  | [] -> new_lst
  | h::t -> (
    match i with
    | 0 -> deli (i - 1) t new_lst
    | _ -> deli (i - 1) t (new_lst@[h])
  )

let writeList strs =
  let out = open_out "pouet" in
  let str = String.concat "\n" strs in
  Printf.fprintf out "%s" str;
  close_out out

let rec read_file input strs =
  let line =
    try Some (input_line input) with
    | End_of_file _ -> None
  in
  match line with
  | None -> (
    close_in input;
    strs
    )
  | Some str -> read_file input (strs@[str])

let getAllJokes () =
  let file =
    try Some (open_in "pouet") with
    | Sys_error _ -> None
  in
  match file with
  | None -> []
  | Some s -> read_file s []

let delJoke n =
  let jokes = getAllJokes () in
  let len = List.length jokes in
  if len == 0 then
    print_endline "they're is no joke, not fun..."
  else if n < 1 then
    print_endline "what are you trying to do ?"
  else if n > len then
    Printf.printf "they're is only %i jokes, you can't ask to delete the %i'nth\n" len n
  else
    writeList (deli (n - 1) jokes [])

let addJoke joke =
  let jokes = (getAllJokes ())@[joke] in
  writeList jokes

let getJoke n =
  let jokes = getAllJokes () in
  let len = List.length jokes in
  if len == 0 then
    print_endline "they're is no joke, not fun..."
  else if n < 1 then
    print_endline "what are you trying to do ?"
  else if n > len then
    Printf.printf "they're is only %i jokes, you can't ask for the %i'nth\n" len n
  else
    print_endline (List.nth jokes (n - 1))
