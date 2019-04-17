type bet_amount = int
type command =
  | Check
  | Fold
  | Call
  | Bet of bet_amount
  | Raise of bet_amount
  | Stack
  | Quit
  | Save

exception Empty
exception Malformed

let command_to_string = function
  | Check -> "checked!"
  | Fold -> "folded!"
  | Call -> "called!"
  | Bet _ -> "bet!"
  | Raise _ -> "raised!"
  | Stack -> "looked at stack!"
  | Quit -> "quit!"
  | Save -> "saved the game!"

let rec remove_emp_str outlist = function
  | [] -> outlist
  | "" :: t -> remove_emp_str outlist t
  | h :: t -> remove_emp_str (h :: outlist) t

let parse str =
  let stringList = String.split_on_char ' ' str in
  let list_without_spaces = List.rev (remove_emp_str [] stringList) in
  if list_without_spaces = [] then raise Empty
  else
    let head = List.hd list_without_spaces in
    let tail = List.tl list_without_spaces in
    let standalone_commands = ["fold"; "call";"quit";"stack";"check"] in
    let phrase_commands = ["bet";"raise"] in
    if List.length tail > 1 then raise Malformed
    else
    if (List.mem head standalone_commands) && tail <> [] then raise Malformed
    else if (List.mem head phrase_commands) && tail = [] then raise Malformed
    else if head = "check" then Check
    else if head = "stack" then Stack
    else if head = "fold" then Fold
    else if head = "call" then Call
    else if head = "save" then Save
    else if head = "bet" then Bet (tail |> List.hd |> int_of_string)
    else if head = "raise" then Raise (tail |> List.hd |> int_of_string)
    else if head = "quit" then Quit
    else raise Malformed

let empty_list lst = lst = []
