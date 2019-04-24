
(** The type of a [bet_amount]. *)
type bet_amount = int

(** The type [command] represents a player command that is decomposed into
    a verb and possibly an object phrase. *)
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

val command_to_string : command -> string

val parse : string -> command

val empty_list: string list -> bool