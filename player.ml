type player = 
  {
    id: int;
    cards: (Deck.suit * Deck.rank) list;
    money: int;
  }

let id player = player.id

(** [id player] returns the id of the player.
    Requires: [player] is a valid player.
    Example: cards {id = 0; cards = [(Diamonds, Ace)]; money = 5} is 
    [(Diamonds, Ace)] *)
let cards player = player.cards

(** [money player] returns the id of the player.
    Requires: [player] is a valid player.
    Example: money {id = 0; cards = []; money = 5} is 5 *)
let money player = player.money