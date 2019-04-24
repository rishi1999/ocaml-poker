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
    avatar_id: int;
    wins: int;
    losses: int;
    consecutive_wins : int;
    orig_id : int
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

(** [cards player] returns a list of cards that the player has in their hand.
    Requires: [player] is a valid player.
    Example: [cards {... cards = [(Diamonds, Ace)]; ...}] is
    [[(Diamonds, Ace)]]. *)
val cards : player -> Deck.card list

(** [money player] returns the current money  of the player.
    Requires: [player] is a valid player.
    Example: [money {... money = 5; ...}] is [5]. *)
val money : player -> int

(** [avatar_id player] returns the index of the avatar selected by the
    player.
    Requires: [player] is a valid player.
    Example: [avatar_id {... avatar_id = 5; ...}] is [5]. *)
val avatar_id : player -> int

(** [orig_id player] returns the original avatar_id selected by the player.
    Requires: [player] is a valid player.
    Example: [orig_id {... orig_id = 5; ...}] is [5]. *)
val orig_id : player -> int

(** [wins player] returns the total wins of the player.
    Requires: [player] is a valid player.
    Example: [wins {... wins = 5; ...}] is [5]. *)
val wins : player -> int

(** [losses player] returns the total losses of the player.
    Requires: [player] is a valid player.
    Example: [losses {... losses = 5; ...}] is [5]. *)
val losses : player -> int

(** [consec_wins player] returns the number of conseuctive wins of the player.
    Requires: [player] is a valid player.
    Example: [consec_wins {... consec_wins = 5; ...}] is [5]. *)
val consec_wins : player -> int