(* Deck initialization and card choosing *)

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          |Queen | King | Ace

let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen;
             King; Ace]

type suit = Clubs | Diamonds | Hearts | Spades

let suits = [Clubs; Diamonds; Hearts; Spades]

type card = suit * rank

(* decks are maintained as lists*)
let played_cards = ref []
let current_deck = ref []
let deck_size = ref 0

let all_ranks (suit:suit) = List.map (fun rank -> (suit,rank)) ranks

let deck = List.concat (List.map (fun suit -> all_ranks suit) suits)

(** [shuffle_deck] returns a list of the full 52 card deck with the cards in
    a randomized order *)
let shuffle_list compare list =
  Random.self_init();
  let tagged_list = List.map (fun card -> (card, Random.bits())) deck in
  let sorted_tags = List.sort compare tagged_list in
  List.map fst sorted_tags

(** [shuffle_deck] shuffles the cards currently in the deck *)
let shuffle_deck () =
  Random.self_init();
  let compare_second a b = Pervasives.compare (snd a) (snd b) in
  let shuffled = shuffle_list compare_second !current_deck in
  current_deck := shuffled

(** [deck_init] initializes the deck all 52 cards *)
let deck_init () =
  current_deck := deck;
  played_cards := [];
  deck_size := 52

(*
(** [deck_size] returns the number of cards remaining in the current
    deck *)
let deck_size = List.length (!current_deck) *)

(** [update_state lst] is the deck with all cards played
    since the last update, [lst],
    removed from the deck and included in the cards-played list.  *)
let update_state card_list =
  played_cards := card_list @ !played_cards;
  current_deck := List.filter (fun x -> not (List.mem x card_list)) !current_deck;
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
   Example int_converter (Clubs, Nine) = 28
   Requires: card is a valid (suit, rank) tuple  *)
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
(*
let update_state card_list =
(*
  match card_list with
  | [] -> ()
  | None :: t -> ()
  | Some a :: t ->
    let extract_cards = List.map (fun x -> match x with | None -> (Clubs,Two) | Some a -> a) card_list in
*)
  current_deck := List.filter (fun x -> not (List.mem x extract_cards)) !current_deck

(** [pick_card] returns None if the current deck is empty and Some card
    if the deck is non empty and card is at the top of the deck*)
let pick_card =
  let temp = !current_deck in
  let card = match temp with
    | [] -> None
    | h :: t -> Some h in
  let state_update = update_state [card] in
  card
*)
(*
(** [pick_rand_card current_deck] returns a random card from current_deck] *)
let pick_rand_card current_deck =

  Random.self_init();
  let rand_int = Random.int (List.length current_deck) - 1 in
  List.nth current_deck rand_int
*)

(* [pick_cards num] returns [num] cards at the top of the current deck. If the
   deck has insufficient cards then num_cards returns None *)
