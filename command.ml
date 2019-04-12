type bet_amount = int
type command =
  | Fold
  | Call
  | Bet of bet_amount
  | Raise of bet_amount
  | Ante
  | Stack
  | Quit

exception Empty
exception Malformed

let rec list_without_empty_str outlist = function
  | [] -> outlist
  | "" :: t -> list_without_empty_str outlist t
  | h :: t -> list_without_empty_str (h :: outlist) t

let is_empty_list = function
  | [] -> true
  | h :: t -> false

let parse str =
  let stringList = String.split_on_char ' ' str in
  let list_without_spaces = List.rev (list_without_empty_str [] stringList) in
  if is_empty_list list_without_spaces then raise Empty
  else
    let head = List.hd list_without_spaces in
    let tail = List.tl list_without_spaces in
    let standalone_commands = ["quit"; "inventory";"score"] in
    let phrase_commands = ["go";"take";"drop";"lock";"unlock"] in
    if List.length tail > 1 then raise Malformed
    else
    if (List.mem head standalone_commands) && tail <> [] then raise Malformed
    else if (List.mem head phrase_commands) && tail = [] then raise Malformed
    else if head = "fold" then Fold
    else if head = "call" then Call
    else if head = "bet" then Bet (tail |> List.hd |> int_of_string)
    else if head = "raise" then Raise (tail |> List.hd |> int_of_string)
    else if head = "ante" then Ante
    else if head = "quit" then Quit
    else raise Malformed
