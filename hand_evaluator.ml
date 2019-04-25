(* Takes in a hand of 5-7 cards and returns its rank *)
open Hashone
open Tableflush
open Constantarrays
open Deck


(* The algorithm here was adapted and improved from the original authored
   in C by Henry R Lee.
   Original Source: https://github.com/HenryRLee/PokerHandEvaluator *)

(** [seven_eval c1 c2 c3 c4 c5 c6 c7] takes in a hand of cards [c1] to [c7] 
    ,each in its integer representation returned by [int_converter] and returns 
    the rank of the hand in terms of which of the 7462 poker equivalence
    classes the hand maps to
    Requires: All input parameters are integers between 1 and 52.*)
let seven_eval c1 c2 c3 c4 c5 c6 c7 =
  let dp = Constantarrays.dynamicProgram in
  let suits = Constantarrays.suits_map in
  let base_5_hash q len k =
    let rec loop i len sum k =
      if i = len then sum
      else if k <=0 then sum 
      else
        let newk = k - (q.(i)) in
        let index1array = dp.(q.(i)) in
        let index2array = index1array.(len - i -1) in
        let new_sum = sum + index2array.(k) in loop (i+1) len new_sum newk in
    loop 0 len 0 k in
  let bin_id = [|
    0x1;	0x1;	0x1;	0x1;
    0x2;	0x2;	0x2;	0x2;
    0x4;	0x4;	0x4;	0x4;
    0x8;	0x8;	0x8;	0x8;
    0x10;	0x10;	0x10;	0x10;
    0x20;	0x20;	0x20;	0x20;
    0x40;	0x40;	0x40;	0x40;
    0x80;	0x80;	0x80;	0x80;
    0x100;	0x100;	0x100;	0x100;
    0x200;	0x200;	0x200;	0x200;
    0x400;	0x400;	0x400;	0x400;
    0x800;	0x800;	0x800;	0x800;
    0x1000;	0x1000;	0x1000;	0x1000;
  |] in

  let suit_bits_ids = [|
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200;
    0x1;	0x8;	0x40;	0x200; |] in

  let suit_hash = suit_bits_ids.(c1) + suit_bits_ids.(c2) + suit_bits_ids.(c3) 
                  + suit_bits_ids.(c4) + suit_bits_ids.(c5) 
                  + suit_bits_ids.(c6) + suit_bits_ids.(c7) in

  let suit_bin = Array.make 4 0 in
  let quinary = Array.make 13 0 in

  if (suits.(suit_hash)) <> 0 then
    let () = suit_bin.(c1 land 0x3) <- 
        suit_bin.(c1 land 0x3) lor bin_id.(c1) in
    let () = suit_bin.(c2 land 0x3) <- 
        suit_bin.(c2 land 0x3) lor bin_id.(c2) in
    let () = suit_bin.(c3 land 0x3) <- 
        suit_bin.(c3 land 0x3) lor bin_id.(c3) in
    let () = suit_bin.(c4 land 0x3) <- 
        suit_bin.(c4 land 0x3) lor bin_id.(c4) in
    let () = suit_bin.(c5 land 0x3) <- 
        suit_bin.(c5 land 0x3) lor bin_id.(c5) in
    let () = suit_bin.(c6 land 0x3) <- 
        suit_bin.(c6 land 0x3) lor bin_id.(c6) in
    let () = suit_bin.(c7 land 0x3) <- 
        suit_bin.(c7 land 0x3) lor bin_id.(c7) in
    let target_index = suits.(suit_hash) - 1 in
    flush_hashes.(suit_bin.(target_index))
  else
    let () = quinary.(c1 lsr 2) <- (quinary.(c1 lsr 2)) + 1 in
    let () = quinary.(c2 lsr 2) <- (quinary.(c2 lsr 2)) + 1 in
    let () = quinary.(c3 lsr 2) <- (quinary.(c3 lsr 2)) + 1 in
    let () = quinary.(c4 lsr 2) <- (quinary.(c4 lsr 2)) + 1 in
    let () = quinary.(c5 lsr 2) <- (quinary.(c5 lsr 2)) + 1 in
    let () = quinary.(c6 lsr 2) <- (quinary.(c6 lsr 2)) + 1 in  
    let () = quinary.(c7 lsr 2) <- (quinary.(c7 lsr 2)) + 1 in
    let hash = base_5_hash quinary 13 7 in
    noflush_hashes.(hash)


(** [seven_list_eval hand] gives the ranking of [hand] in terms of which of
    the 7462 equivalence classes of 7 card poker it corresponds to. 
    Lower values denote a more powerful rank.
    Example: [seven_list_eval [(Hearts,Two);(Hearts,Three);(Spades,Four);
    (Diamonds,Five);(Diamonds,Ace);(Diamonds,Ten);(Diamonds,King)]] is [1609]
    Requires: [hand] comprises 7 unique cards represented as (suit, rank)
    tuples. *)
let seven_list_eval hand = 
  let a = Deck.int_converter (List.nth hand 0) in
  let b = Deck.int_converter (List.nth hand 1) in
  let c = Deck.int_converter (List.nth hand 2) in
  let d = Deck.int_converter (List.nth hand 3) in
  let e = Deck.int_converter (List.nth hand 4) in
  let f = Deck.int_converter (List.nth hand 5) in
  let g = Deck.int_converter (List.nth hand 6) in
  seven_eval a b c d e f g

(** [seven_int_list_eval hand] returns the rank of [hand] in terms of which of
    the 7462 equivalence classes of 7 card poker it corresponds to. 
    Lower values denote a more powerful rank.
    Example: [seven_int_list_eval [1;2;3;4;5;6;7] is [154].]
    Requires: [hand] is a list of 7 unique cards represented as distinct
    integers from 0 to 1 inclusive.  *)
let seven_int_list_eval hand = 
  let a = List.nth hand 0 in
  let b = List.nth hand 1 in
  let c = List.nth hand 2 in
  let d = List.nth hand 3 in
  let e = List.nth hand 4 in
  let f = List.nth hand 5 in
  let g = List.nth hand 6 in
  seven_eval a b c d e f g


(** [rank_mapper rank_val] returns the general name of the rank of a hand that
    has been mapped to [rank_val] using the evaluator function for the hand.
    Example: [rank_mapper 1] is "Royal Flush" (as the hand has been mapped
    to the highest rank so it must contain the Ace, King, Queen, Jack and Ten
    of a suit.
    Requires: [rank_val] is between 1 and 7462)  *)
let rank_mapper rank_val =  if rank_val = 0 then 
    "Nothing (because everyone else folded)"
  else if rank_val = 1 then "Royal Flush"
  else if rank_val <= 10 then "Straight Flush"
  else if rank_val <= 166 then "Four of a Kind"
  else if rank_val <= 322 then "Full House"
  else if rank_val <= 1599 then "Flush"
  else if rank_val <= 1609 then "Straight"
  else if rank_val <= 2467 then "Three of a Kind"
  else if rank_val <= 3325 then "Two Pair"
  else if rank_val <= 6185 then "One Pair"
  else "High Card"