type player = 
  {
    id: int;
    cards: (Deck.suit * Deck.rank) list;
    money: int;
  }

(** [id player] returns the id of the player.
    Requires: player is a valid player.
    Example: [id {}  *)
let id player = player.id
let cards player = player.cards
let money player = player.money