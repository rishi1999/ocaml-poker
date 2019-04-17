(**Type representing the rank of a card*)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          |Queen | King | Ace

(**Type representing the suit of a card*)
type suit = Clubs | Diamonds | Hearts | Spades

(**Type representing a card*)
type card = suit * rank

val deck_size : int ref

val shuffle_deck : unit -> unit

val shuffle_list : (card * int -> card * int -> int) -> 'a -> card list

val pick_cards : int -> card list

val update_state : card list -> unit

val deck_init : unit -> unit

val int_converter : card -> int

val card_printer : card -> string
