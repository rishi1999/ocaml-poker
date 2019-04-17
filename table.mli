open Deck
open Player

type table = {
  pot: int;
  blind: int;
  participants: Player.player list;
  board: (Deck.card) list;
}

val pot : table -> int

val blind : table -> int

val participants : table -> Player.player list

val board : table -> (Deck.card) list

val next_round_players: table -> table

val deal: table -> table

val add_to_hole:  table -> table

val clear_players: player list -> player list -> player list

val clear_round: table -> table
