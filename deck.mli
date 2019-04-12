(**Type representing the rank of a card*)
type rank

(**Type representing the suit of a card*)
type suit

(**Type representing a card*)
type card


val shuffle_deck : unit

val shuffle_list : ((suit * rank) * int -> (suit * rank) * int -> int) -> 'a -> (suit * rank) list

val pick_card : suit * rank

val pick_cards : int -> (suit * rank) list

val update_state : (suit * rank) list -> unit

val deck_size : int

val deck_init : unit

