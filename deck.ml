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
let shuffle_deck =
  Random.self_init();
  let compare_second a b = Pervasives.compare (snd a) (snd b) in
  let shuffled = shuffle_list compare_second !current_deck in
  current_deck := shuffled

(** [deck_init] initializes the deck all 52 cards *)
let deck_init =
  current_deck := deck;
  played_cards := []

(** [deck_size] returns the number of cards remaining in the current
    deck *)
let deck_size = List.length !current_deck


let update_state card_list =
  played_cards := card_list :: !played_cards;
  current_deck := List.filter (fun x -> not (List.mem x card_list)) !current_deck 

(** [pick_card] returns None if the current deck is empty and Some card 
    if the deck is non empty and card is at the top of the deck.
    Raises: Failure "Deck is empty" if there are no cards in the deck*)
let pick_card =
  if deck_size = 0 then failwith "Deck is empty"
  else 
    let card = List.hd !current_deck in
    let _state_update = update_state [card] in
    card

(** [pick_cards num] returns a list of [num] cards from the top of the deck.
    Raises: Failure "Insufficient Cards" if you attempt to pick more cards
    than currently available in the deck. *)
let pick_cards num = 
  if deck_size < num then failwith  "Insufficient Cards"
  else
    let rec list_builder count outlist a = match a with
      | [] -> outlist
      | h :: t when count = 0 -> outlist
      | h :: t -> list_builder (count - 1) (h :: outlist) t in
    let cards = List.rev (list_builder num [] !current_deck) in
    let _state_update = update_state cards in
    cards

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
