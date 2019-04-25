
(** [rank] is the rank of a card*)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          | Queen | King | Ace

(** [Suit] is the type of a card's suit. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** [card] is a card with a suit and a rank. *)
type card = suit * rank

val current_deck : ((suit * rank) list) ref

val deck_size : int ref

val shuffle_deck : unit -> unit

val pick_cards : int -> card list

val update_state : card list -> unit

val deck_init : unit -> unit

val int_converter : card -> int

val pick_efficient : int -> int list -> int list