(* Deck initialization and card choosing *)

(** [rank] is the rank of a card
    [Two]: corresponds to a rank 2*)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          |Queen | King | Ace

(** The type of a card's suit. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The type of a card in the Deck. *)
type card = suit * rank

(* [played_cards] is a mutable (suit * rank) list ref that is used to track
   which cards have been selected and used from the deck. *)
let played_cards = ref []

(* [current_deck] is a mutable (suit * rank) list ref that is used to track
   which cards are currently in the deck and have not been used. The cards
   at the front of the list are at the top of the deck and would be used
   first. *)
let current_deck = ref []

(* [deck_size] is an int ref that tracks the number of cards currently in
   the deck. *)
let deck_size = ref 0

let deck = let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
                        Jack; Queen; King; Ace] in 
  let all_ranks (suit:suit) = List.map (fun rank -> (suit,rank)) ranks in
  let suits = [Clubs; Diamonds; Hearts; Spades] in
  List.concat (List.map (fun suit -> all_ranks suit) suits)


(** [shuffle_deck] shuffles the cards currently in the deck. *)
let shuffle_deck () =
  Random.self_init();
  let shuffle_list compare list =
    Random.self_init();
    let tagged_list = List.map (fun card -> (card, Random.bits())) deck in
    let sorted_tags = List.sort compare tagged_list in
    List.map fst sorted_tags in
  let compare_second a b = Pervasives.compare (snd a) (snd b) in
  let shuffled = shuffle_list compare_second !current_deck in
  current_deck := shuffled

(** [deck_init] initializes the deck with all 52 cards in the same order
    each time. *)
let deck_init () =
  current_deck := deck;
  played_cards := [];
  deck_size := 52

(** [update_state lst] is the deck with all cards played
    since the last update, [lst],
    removed from the deck and included in the cards-played list.
    Example: update_state [(Club, Five)] removes (Club, Five) from the 
    [current_deck]*)
let update_state card_list =
  played_cards := card_list @ !played_cards;
  current_deck := List.filter (fun x -> not (List.mem x card_list)) 
      !current_deck;
  deck_size := List.length !current_deck

(** [pick_cards num] returns a list of [num] cards from the top of the deck.
    Raises: Failure "Insufficient Cards" if you attempt to pick more cards
    than currently available in the deck. *)
let pick_cards num =
  if !deck_size < num then failwith  "Insufficient Cards"
  else
    let rec list_builder count outlist a = match a with
      | [] -> outlist
      | h :: t when count = 0 -> outlist
      | h :: t -> list_builder (count - 1) (h :: outlist) t in
    let cards = List.rev (list_builder num [] !current_deck) in
    update_state cards;
    cards

(* [int_converter card] returns an integer representation of the card
   such that the card is in the range 0 to 52. The integer is calculcated by
   taking the rank of the card and mapping it from 0 to 12 such that Two maps
   to 0 and Ace to 12 and the suit of the card (0 for Clubs, 1 for Diamonds,
   2 for Hearts and 3 for Spades). The card is then represented as
   Rank * 4 + Suit.
   Example: int_converter (Clubs, Nine) = 28
   Requires: [card] is a valid (suit, rank) tuple  *)
let int_converter card =
  let offset = match card with
    | Clubs, _-> 0
    | (Diamonds, _) -> 1
    | (Hearts, _) -> 2
    | (Spades, _) -> 3 in
  let rank = match card with
    | (_, Two) -> 0
    | (_, Three) -> 1
    | (_, Four) -> 2
    | (_, Five) -> 3
    | (_, Six) -> 4
    | (_, Seven) -> 5
    | (_, Eight) -> 6
    | (_, Nine) -> 7
    | (_, Ten) -> 8
    | (_, Jack) -> 9
    | (_, Queen) -> 10
    | (_, King) -> 11
    | (_, Ace) -> 12 in
  rank * 4 + offset
