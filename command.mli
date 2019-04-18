
(* The type of a bet amount. *)
type bet_amount = int

(* The type of a command *)
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

val empty_list: string list -> bool