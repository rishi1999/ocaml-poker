
type action = Fold | Call | Raise | Small | Big | Ante | None

type player = 
  {
    id: int;
    action: action;
    cards: (Deck.suit * Deck.rank) list;
    money: int;
  }