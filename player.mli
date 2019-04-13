type action = Fold | Call | Raise | Small | Big | Ante

type player =
  {
    action: action;
    name: string;
    cards: (Deck.suit * Deck.rank) list;
    money: int;
  }