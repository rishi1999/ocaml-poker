type player =
  {
    id: int;
    cards: Deck.card list;
    money: int;
  }

let id player = player.id
let cards player = player.cards
let money player = player.money