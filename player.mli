(** [id] : the id of the player
    [cards] : the cards that the player is currently holding
    [money] : the amount of money the player currently has
*)
type player =
  {
    id: int;
    name: string;
    cards: Deck.card list;
    money: int;
  }

(** [id player] returns the id of the player.
    Requires: [player] is a valid player.
    Example: [id {... id = 3; ...}] is
    [3]. *)
val id : player -> int

(** [name player] returns the name of the player.
    Requires: [player] is a valid player.
    Example: [name {... name = "Nate"; ...}] is
    ["Nate"]. *)
val name : player -> string

(** [cards player] returns the name of the player.
    Requires: [player] is a valid player.
    Example: [cards {... cards = [(Diamonds, Ace)]; ...}] is
    [[(Diamonds, Ace)]]. *)
val cards : player -> Deck.card list

(** [money player] returns the id of the player.
    Requires: [player] is a valid player.
    Example: [money {... money = 5; ...}] is [5]. *)
val money : player -> int