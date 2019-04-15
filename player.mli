type player =
  {
    id: int;
    cards: (Deck.suit * Deck.rank) list;
    money: int;
  }

val id : player -> int
val cards : player -> (Deck.suit * Deck.rank) list
val money : player -> int