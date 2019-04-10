(* Deck initialization and card choosing *)

open State

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
          |Queen | King | Ace

let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen;
             King; Ace]

type suit = Clubs | Diamonds | Hearts | Spades

let suits = [Clubs; Diamonds; Hearts; Spades]

type card = suit * rank

let all_ranks suit = List.map (fun rank -> (suit,rank)) ranks

let deck = List.concat (List.map (fun suit -> all_ranks suit) suits)

(* [shuffle_deck] returns a list of the full 52 card deck with the cards in
   a randomized order *)
let shuffle_deck =
  Random.self_init();
  let compare_second a b = Pervasives.compare (snd a) (snd b) in
  let tagged_list = List.map (fun card -> (card, Random.bits())) deck in
  let sorted_tags = List.sort compare_second tagged_list in
  List.map fst sorted_tags

(** [pick_card current_deck] returns a random card from current_deck] *)
let pick_card current_deck =

  Random.self_init();
  let rand_int = Random.int (List.length current_deck) - 1 in
  List.nth current_deck rand_int 
