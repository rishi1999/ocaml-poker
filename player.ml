type player =
  {
    id: int;
    name: string;
    cards: Deck.card list;
    money: int;
  }

let id player = player.id
let name player = player.name
let cards player = player.cards
let money player = player.money