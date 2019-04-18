type player =
  {
    id: int;
    cards: Deck.card list;
    money: int;
  }

val id : player -> int
val cards : player -> Deck.card list
val money : player -> int