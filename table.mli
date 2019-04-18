open Deck
open Player

(** [table] represents a poker table described using the following
    information:
    [pot] : money accumulated from bets and blinds
    [blind] : integer id of player who is blind for the current round
    [participants] : list of players in the game
    [board] : cards on the board *)
type table = {
  pot: int;
  blind: int;
  participants: Player.player list;
  board: Deck.card list;
}

(** [pot tab] returns the total current pot of [tab]
    Requires: [tab] is a valid table
    Example: [pot {}]*)
val pot : table -> int

(** [blind tab] returns the blind player number of [tab]
    Requires: [tab] is a valid table*)
val blind : table -> int

(** [participants tab] returns the current player list of table [tab]
    Requires: [tab] is a valid table*)
val participants : table -> Player.player list

(** [board tab] returns the board card list of table [tab]
    Requires [tab] is a valid table*)
val board : table -> (Deck.card) list

(** [next_round_players tab] returns the table [tab] with blind
     updated for the next round
    Requires: [tab] is a valid table*)
val next_round_players: table -> table

(** [deal tab] deals 2 cards to each player in [tab] and returns it
    Requires: [tab] is a valid table
    Requires: each participant in [tab] has 0 cards
    Raises: "player has non 0 cards" exception if any player doesn't have 
    0 cards*)
val deal: table -> table

(** [add_to_hole tab] adds cards to the board depending on
    how many are already there
    Requires: [tab] is a valid table*)
val add_to_board:  table -> table

(** [clear_players p] clears the cards of each player in player list [p]
    Requires: [p] is a valid player list*)
val clear_players: player list -> player list -> player list

(** [clear_round table x] is the table [x] with the cards cleared.
    Requires: [x] is a valid table*)
val clear_round: table -> table
