(* Takes in a hand of 5-7 cards and returns its rank *)

(*
Cards are stored in a 32 bit word which has the following (implied) structure:
    struct card{
       unsigned num_A:2;
       unsigned num_K:2;
       unsigned num_Q:2;
       //....
       unsigned num_2:2;
       unsigned spare:2;
       unsigned spade:1;
       unsigned heart:1;
       unsigned diamond:1;
       unsigned club:1;
       };

*)

(* [pow b e] returns the integer result of b raised to the power of e
   Requires: e >= 0 *)
let rec pow b e = match e with
  | 0 -> 1
  | 1 -> b
  | n -> 
    let c = pow b (n / 2) in
    c * c * (if n mod 2 = 0 then 1 else b)

(** [hand_evaluator cs ss] returns the integer position of the 5 cards
    cs in the rank array hands 
    hands=["4 of a Kind", "Straight Flush", "Straight", "Flush", "High Card",
       "1 Pair", "2 Pair", "Royal Flush", "3 of a Kind", "Full House" ]; *)
let hand_evaluator cs ss =
  let s = (1 lsl cs.(0)) lor (1 lsl cs.(1)) lor (1 lsl cs.(2)) lor (1 lsl cs.(3)) lor (1 lsl cs.(4)) in
  let rec loop i v o = 
    if i = 5 then v
    else if o = 0 then loop (i + 1) (v + 1) (pow 2 (cs.(i)*4))
    else loop (i + 1) (v + o * ( (v / o) land 15) + 1) (pow 2 (cs.(i)*4)) in
  let v = loop 0 0 (pow 2 (cs.(0)*4)) in
  let newv = if s land 1 = 0 || s = 0x403c then v mod 15 - 3
    else v mod 15 - if (s / ( s land (1)) = 31) || (s = 0x403c) then 3 else 1 in
  let multiplier = if s = 0x7c00 then -5 else 1 in
  let boolean = if ss.(0) = ((ss.(1) lor ss.(2) lor ss.(3) lor ss.(4))) then 1 else 0 in
  newv - boolean * multiplier