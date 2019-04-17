(* Takes in a hand of 5-7 cards and returns its rank *)
open Hashone
open Tableflush
open Constantarrays
open Deck

(* [pow b e] returns the integer result of b raised to the power of e
   Requires: e >= 0 *)
let rec pow b e = match e with
  | 0 -> 1
  | 1 -> b
  | n -> 
    let c = pow b (n / 2) in
    c * c * (if n mod 2 = 0 then 1 else b)

(** [hand_evaluator card_ranks card_suits] returns the integer position of the 
    5 cards as in the rank array hands given below
    hands=["4 of a Kind", "Straight Flush", "Straight", "Flush", "High Card",
       "1 Pair", "2 Pair", "Royal Flush", "3 of a Kind", "Full House" ]; *)
let hand_evaluator card_ranks card_suits =
  let s = (1 lsl card_ranks.(0)) lor (1 lsl card_ranks.(1)) 
          lor (1 lsl card_ranks.(2)) lor (1 lsl card_ranks.(3)) 
          lor (1 lsl card_ranks.(4)) in
  let rec loop i v o = 
    if i = 5 then v
    else if o = 0 then loop (i + 1) (v + 1) (pow 2 (card_ranks.(i)*4))
    else loop (i + 1) (v + o * ( (v / o) land 15) + 1) 
        (pow 2 (card_ranks.(i)*4)) in
  let v = loop 0 0 (pow 2 (card_ranks.(0)*4)) in
  let newv = if s land 1 = 0 || s = 0x403c then v mod 15 - 3
    else v mod 15 - if (s / ( s land (1)) = 31) || (s = 0x403c) then 3 
         else 1 in
  let multiplier = if s = 0x7c00 then -5 else 1 in
  let boolean = if card_suits.(0) = ((card_suits.(1) lor card_suits.(2) lor 
                                      card_suits.(3) lor card_suits.(4))) 
    then 1 
    else 0 in
  newv - boolean * multiplier


let dp = Constantarrays.dp
let suits = Constantarrays.suits

let hash_quinary q len k =
  let rec loop i len sum k =
    if i = len then sum
    else if k <=0 then sum 
    else
      let newk = k - (q.(i)) in
      let index1array = dp.(q.(i)) in
      let index2array = index1array.(len - i -1) in
      let new_sum = sum + index2array.(k) in loop (i+1) len new_sum newk in
  loop 0 len 0 k

(* 7 card evaluator starts here *)

let seven_eval a b c d e f g =

  let binaries_by_id = [|
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

  let suitbit_by_id = [|
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

  let suit_hash = suitbit_by_id.(a) + suitbit_by_id.(b) + suitbit_by_id.(c) + 
                  suitbit_by_id.(d) + suitbit_by_id.(e) + suitbit_by_id.(f) + 
                  suitbit_by_id.(g) in

  let suit_binary = Array.make 4 0 in
  let quinary = Array.make 13 0 in

  if (suits.(suit_hash)) <> 0 then
    let () = suit_binary.(a land 0x3) <- 
        suit_binary.(a land 0x3) lor binaries_by_id.(a) in
    let () = suit_binary.(b land 0x3) <- 
        suit_binary.(b land 0x3) lor binaries_by_id.(b) in
    let () = suit_binary.(c land 0x3) <- 
        suit_binary.(c land 0x3) lor binaries_by_id.(c) in
    let () = suit_binary.(d land 0x3) <- 
        suit_binary.(d land 0x3) lor binaries_by_id.(d) in
    let () = suit_binary.(e land 0x3) <- 
        suit_binary.(e land 0x3) lor binaries_by_id.(e) in
    let () = suit_binary.(f land 0x3) <- 
        suit_binary.(f land 0x3) lor binaries_by_id.(f) in
    let () = suit_binary.(g land 0x3) <- 
        suit_binary.(g land 0x3) lor binaries_by_id.(g) in
    let target_index = suits.(suit_hash) - 1 in
    flush.(suit_binary.(target_index))
  else
    let () = quinary.(a lsr 2) <- (quinary.(a lsr 2)) + 1 in
    let () = quinary.(b lsr 2) <- (quinary.(b lsr 2)) + 1 in
    let () = quinary.(c lsr 2) <- (quinary.(c lsr 2)) + 1 in
    let () = quinary.(d lsr 2) <- (quinary.(d lsr 2)) + 1 in
    let () = quinary.(e lsr 2) <- (quinary.(e lsr 2)) + 1 in
    let () = quinary.(f lsr 2) <- (quinary.(f lsr 2)) + 1 in  
    let () = quinary.(g lsr 2) <- (quinary.(g lsr 2)) + 1 in
    let hash = hash_quinary quinary 13 7 in
    noflush.(hash)

(*REWRITE THIS FOR EFFICIENCY. NO NEED TO ITERATE AGAIN AND AGAIN OVER
  THE SAME LIST. USE RECURSION. *)
let seven_list_eval (hand:(Deck.suit * Deck.rank) list) = 
  if List.length hand <> 7 then failwith "not 7 cards";
  let a = Deck.int_converter (List.nth hand 0) in
  let b = Deck.int_converter (List.nth hand 1) in
  let c = Deck.int_converter (List.nth hand 2) in
  let d = Deck.int_converter (List.nth hand 3) in
  let e = Deck.int_converter (List.nth hand 4) in
  let f = Deck.int_converter (List.nth hand 5) in
  let g = Deck.int_converter (List.nth hand 6) in
  seven_eval a b c d e f g

let rank_mapper rank = if rank = 1 then "Royal Flush"
  else if rank >= 10 then "Straight Flush"
  else if rank >= 166 then "Four of a Kind"
  else if rank >= 322 then "Full House"
  else if rank >= 1599 then "Flush"
  else if rank >= 1609 then "Straight"
  else if rank >= 2467 then "Three of a Kind"
  else if rank >= 3325 then "Two Pair"
  else if rank >= 6185 then "One Pair"
  else "High Card"