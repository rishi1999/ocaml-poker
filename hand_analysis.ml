open Deck

let custom_rank_converter card = match card with
  | Two -> 1.
  | Three -> 1.5
  | Four -> 2.
  | Five -> 2.5
  | Six -> 3.
  | Seven -> 3.5
  | Eight -> 4.
  | Nine -> 4.5
  | Ten -> 5.
  | Jack -> 6.
  | Queen -> 7.
  | King -> 8.
  | Ace -> 10.

let rank_converter card = match card with
  | Two -> 2.
  | Three -> 3.
  | Four -> 4.
  | Five -> 5.
  | Six -> 6.
  | Seven -> 7.
  | Eight -> 8.
  | Nine -> 9.
  | Ten -> 10.
  | Jack -> 11.
  | Queen -> 12.
  | King -> 13.
  | Ace -> 14.

let chen_formula hole = 
  let card1 = List.hd hole in
  let card2 = List.nth hole 1 in
  let suit1 = fst card1 in
  let suit2 = fst card2 in
  let rank1 = custom_rank_converter (snd card1) in
  let rank2 = custom_rank_converter (snd card2) in
  let ord_rank1 = rank_converter (snd card1) in
  let ord_rank2 = rank_converter (snd card2) in
  let pair = if rank1 = rank2 then 2. else 1. in
  let highest_score =  max rank1 rank2 in
  let suited = if suit1 = suit2 then 2. else 0. in
  let gap = (abs_float (ord_rank1 -. ord_rank2)) -. 1. in
  let gap_penalty = if gap >= 4. then -5. else if gap = 3. then -4.
    else if gap >= 1. then -1. *. gap else 0. in
  let straight_bonus = if rank1 < 7. && rank2 < 7. && gap <= 1. && pair = 1.  then 1. else 0. in
  ceil ((highest_score *. pair) +. suited +. gap_penalty +. straight_bonus)