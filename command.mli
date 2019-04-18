type bet_amount = int

type command =
  | Check
  | Fold
  | Call
  | Bet of bet_amount
  | Raise of bet_amount
  | Stack
  | Quit
  (* | Save *)

exception Empty
exception Malformed

val command_to_string : command -> string

val parse : string -> command

val remove_emp_str: string list -> string list -> string list

val empty_list: string list -> bool