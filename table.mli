open Deck
open Seats
open Player

type table = {
  dealer: int;
  blind: int;
  participants: seats;
  hole_cards: (Deck.suit * Deck.rank) list;
}

val next_round_players: table -> table

val deal: table -> table

val add_to_hole:  table -> table

val clear_players: player list -> player list -> player list

val clear_round: table -> table
