(** [id] : the id of the player
    [cards] : the cards that the player is currently holding
    [money] : the amount of money the player currently has
*)
type player =
  {
    id: int;
    cards: Deck.card list;
    money: int;
  }
(** [id player] returns the id of the player.
    Requires: player is a valid player. *)
val id : player -> int

(** [cards player] returns the cards that the player has.
    Requires: player is a valid player. *)
val cards : player -> Deck.card list

(** [money player] returns how much the player has.
    Requires: player is a valid player. *)
val money : player -> int