type player =
  {
    id: int;
    name: string;
    cards: Deck.card list;
    money: int;
    avatar_id: int;
    wins: int;
    losses: int;
  }

let id player = player.id
let name player = player.name
let cards player = player.cards
let money player = player.money
let avatar_id player = player.avatar_id
let wins player = player.wins
let losses player = player.losses