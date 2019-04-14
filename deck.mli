(**Type representing the rank of a card*)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          |Queen | King | Ace

(**Type representing the suit of a card*)
type suit = Clubs | Diamonds | Hearts | Spades

(**Type representing a card*)
type card

val shuffle_deck : unit

val shuffle_list : ((suit * rank) * int -> (suit * rank) * int -> int) -> 'a -> (suit * rank) list

val pick_card : suit * rank

val pick_cards : int -> (suit * rank) list

val update_state : (suit * rank) list -> unit

val deck_size : int

val deck_init : unit

val int_converter : suit * rank -> int

