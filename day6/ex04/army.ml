class ['a] army (members: 'a list)=
object (self)

  val mutable _members = members

  method add unity =
    _members <- unity::_members

  method delete =
    _members <- match _members with
                | [] -> (
                  print_endline "army decimated";
                  []
                )
                | h::t -> t

  method isDeadsaChacal =
    match _members with
    | [] -> true
    | _ -> false

end