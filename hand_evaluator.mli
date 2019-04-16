


(** [seven_list_eval hand] gives ranking of hand
    Requires hand has 7 cards
    throws "not 7 cards" exception if hand does not have 7 cards*)
val seven_list_eval : (Deck.suit * Deck.rank) list -> int

(** [get_nth (lst, n)] returns the nth value of lst
      Requires n is less than length of list - 1*)
val get_nth : 'a list * int -> 'a