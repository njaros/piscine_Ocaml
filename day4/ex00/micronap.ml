let my_sleep () = Unix.sleep 1

let () =
  let len = Array.length Sys.argv in
  match len with
  | 2 -> (
    match int_of_string_opt Sys.argv.(1) with
    | None -> ()
    | Some n -> (
      if n <= 0 then ()
      else (
        for i = 1 to n do
          my_sleep ()
        done
        )
    )
  )
  | _ -> ()
