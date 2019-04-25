(* Deck initialization and card choosing *)

(** [rank] is the rank of a card*)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          | Queen | King | Ace

(** [suit] is the type of a card's suit. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** [card] is a card with a suit and a rank*)
type card = suit * rank

(* [played_cards] is a mutable (suit * rank) list ref that is used to track
   which cards have been selected and used from the deck. *)
let played_cards = ref []

(** [current_deck] is a mutable (suit * rank) list ref that is used to track
    which cards are currently in the deck and have not been used. The cards
    at the front of the list are at the top of the deck and would be used
    first. *)
let current_deck = ref []

(** [deck_size] is an int ref that tracks the number of cards currently in
    the deck. *)
let deck_size = ref 0

let deck = let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
                        Jack; Queen; King; Ace] in 
  let all_ranks (suit:suit) = List.map (fun rank -> (suit,rank)) ranks in
  let suits = [Clubs; Diamonds; Hearts; Spades] in
  List.concat (List.map (fun suit -> all_ranks suit) suits)

let constant_deck =  deck

(* [cons_int_deck] is a list of integers from 0 to 51 that represents
   all 52 cards in a deck as follows: 
   We take the rank of each card and
   map it to an integer such that Two maps to 0 and Ace maps to 12 (with the 
   remaining cards being mapped in a corresponding order) and
   then match the suits such that Clubs maps to 0, Diamonds to 1, Hearts to 2
   and Spades to 3. Then the integer representation of the card is
   rank * 4 + suit. *)
let const_int_deck =  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 
                       16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 
                       29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41;
                       42; 43; 44; 45; 46; 47; 48;49; 50; 51]

(** [pick_efficent number used] returns a list containing [number] random cards
    from a deck does not contain any [used] cards.
    Example: [pick_efficient 1 (Spades,Ace)] could be [(Hearts, Three)].
    Note that we say could be and not guarranteed to be as pick_efficient
    randomly initializes the seed value for the random number generator
    before shuffling the deck.
    Requires: [used] is an int list comprising integers from 0 (inclusive) to
    51 (inclusive).
    Requires: [number] is positive. *)
let pick_efficient number used =
  let new_deck = List.filter (fun x -> not (List.mem x used)) const_int_deck in
  let shuffle_list compare list =
    Random.self_init();
    let tagged_list = List.map (fun card -> (card, Random.bits())) list in
    let sorted_tags = List.sort compare tagged_list in
    List.map fst sorted_tags in
  let compare_second a b = Pervasives.compare (snd a) (snd b) in
  let shuffled_new = shuffle_list compare_second new_deck in
  let rec list_builder count outlist a = match a with
    | [] -> outlist
    | h :: t when count = 0 -> outlist
    | h :: t -> list_builder (count - 1) (h :: outlist) t in
  List.rev (list_builder number [] shuffled_new)


(** [shuffle_deck ()] shuffles the cards currently in the deck. *)
let shuffle_deck () =
  let shuffle_list compare list =
    Random.self_init();
    let tagged_list = List.map (fun card -> (card, Random.bits())) list in
    let sorted_tags = List.sort compare tagged_list in
    List.map fst sorted_tags in
  let compare_second a b = Pervasives.compare (snd a) (snd b) in
  let shuffled = shuffle_list compare_second !current_deck in
  current_deck := shuffled

(** [deck_init ()] initializes the deck with all 52 cards in the same order
    each time. *)
let deck_init () =
  current_deck := deck;
  played_cards := [];
  deck_size := 52

(** [update_state lst] is the deck with all cards played
    since the last update, [lst],
    removed from the deck and included in the cards-played list.
    Example: update_state [(Club, Five)] removes (Club, Five) from the
    [current_deck] *)
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

(* [int_converter card] returns an integer representation of [card]
   such that [card] is in the range 0 to 52. The integer is calculcated by
   taking the rank of [card] and mapping it from 0 to 12 such that Two maps
   to 0 and Ace to 12 and the suit of the card (0 for Clubs, 1 for Diamonds,
   2 for Hearts and 3 for Spades). [card] is then represented as
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
