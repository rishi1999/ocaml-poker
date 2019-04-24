type bet_amount = int

type command =
  | Check
  | Fold
  | Call
  | Bet of bet_amount
  | Raise of bet_amount
  | Stack
  | Save
  | Show
  | Quit

exception Empty
exception Malformed

(** [command_to_string command] takes in a command and returns the
    string representation of it.
    Requires: command is a valid command
    Example: [command_to_string Check] is "checked!" *)
let command_to_string = function
  | Show -> failwith "Show should not be printed"
  | Save -> failwith "Save should not be printed"
  | Check -> "checked!"
  | Fold -> "folded!"
  | Call -> "called!"
  | Bet x -> "bet $" ^ (string_of_int x) ^ "!"
  | Raise x -> "raised $" ^ (string_of_int x) ^ "!"
  | Stack -> failwith "Stack should not be printed"
  | Quit -> failwith "Quit should not be printed"

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words,
    if any, become the object phrase.
    Examples:
    - [parse "    bet 10  "] is [Bet "10"]
    - [parse "quit"] is [Quit].
    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Raises: [Empty] if [str] is the empty string or contains only spaces.
    Raises: [Malformed] if the command is malformed. A command
    is malformed if the verb is neither "fold" nor "call" nor "quit" nor
    "stack" nor "check" nor "bet" nor "raise"
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "call" and there is a non-empty object phrase,
    or if the verb is "show" and there is a non-empty object phrase,
    or if the verb is "save" and there is a non-empty object phrase,
    or if the verb is "check" and there is a non-empty object phrase,
    or if the verb is "stack" and there is a non-empty object phrase,
    or if the verb is "fold" and there is a non-empty object phrase,
    or if the verb is "bet" and there is an empty object phrase.
    or if the verb is "raise" and there is an empty object phrase. *)
let parse str =
  let rec remove_emp_str outlist = function
    | [] -> outlist
    | "" :: t -> remove_emp_str outlist t
    | h :: t -> remove_emp_str (h :: outlist) t in
  let stringList = String.split_on_char ' ' str in
  let list_without_spaces = List.rev (remove_emp_str [] stringList) in
  if list_without_spaces = [] then raise Empty
  else
    let head = List.hd list_without_spaces in
    let tail = List.tl list_without_spaces in
    let standalone_commands = ["fold"; "call";"quit";"stack";
                               "show";"check"; "save"] in
    let phrase_commands = ["bet";"raise"] in
    if List.length tail > 1 then raise Malformed
    else
    if (List.mem head standalone_commands) && tail <> [] then raise Malformed
    else if (List.mem head phrase_commands) && tail = [] then raise Malformed
    else if head = "check" then Check
    else if head = "stack" then Stack
    else if head = "fold" then Fold
    else if head = "call" then Call
    else if head = "show" then Show
    else if head = "save" then Save
    else if head = "bet" then
      try Bet (tail |> List.hd |> int_of_string) with
      | Failure _ -> raise Malformed
    else if head = "raise" then
      try Raise (tail |> List.hd |> int_of_string) with
      | Failure _ -> raise Malformed

    else if head = "quit" then Quit
    else raise Malformed

let empty_list lst = lst = []
