type bet_amount = int

type command = 
  | Check
  | Fold
  | Call
  | Bet of bet_amount
  | Raise of bet_amount
  | Stack
  | Quit

exception Empty
exception Malformed

val parse : string -> command

val remove_emp_str: string list -> string list -> string list

val empty_list: string list -> bool