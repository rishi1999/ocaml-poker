open Deck
open Player

(** [table] is a table:
*)
type table = {
  pot: int;
  blind: int;
  participants: Player.player list;
  board: Deck.card list;
}

(** [pot tab] returns the total current pot of the table tab
    Requires: valid table tab*)
val pot : table -> int

(** [blind tab] returns the blind player number of the table tab
    Requires tab is a valid table*)
val blind : table -> int

(** [participants tab] returns the current total player list of table tab
    Requires tab is a valid table*)
val participants : table -> Player.player list

(** [board tab] returns the board card list of table tab
    Requires tab is a valid table*)
val board : table -> (Deck.card) list

(** [next_round_players tab] returns the table with blind updated for the next round
    Requires tab is a valid table*)
val next_round_players: table -> table

(** [deal tab] deals 2 cards to each player in the table tab and returns it
    Requires: [tab] is a valid table
    Requires: each participant has 0 cards
    Raises: "player has non 0 cards" exception if any player doesn't have 0 cards*)
val deal: table -> table

(** [add_to_hole tab] adds cards to the board depending on
    how many are already there
    Requires: tab is a valid table*)
val add_to_board:  table -> table

(** [clear_players p] clears the cards of each player in player list [p]
    Requires p is a valid player list*)
val clear_players: player list -> player list -> player list

(** [clear_round table x] is the table x with the cards cleared.
    Requires x is a valid table*)
val clear_round: table -> table
