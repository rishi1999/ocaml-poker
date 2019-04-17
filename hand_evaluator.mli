
(** [seven_list_eval hand] gives ranking of hand
    Requires hand has 7 cards
    throws "not 7 cards" exception if hand does not have 7 cards*)
val seven_list_eval : (Deck.suit * Deck.rank) list -> int

val seven_eval : int -> int -> int -> int -> int -> int -> int -> int

val rank_mapper : int -> string