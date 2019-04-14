(* Takes in a hand of 5-7 cards and returns its rank *)
open Hashone
open Tableflush
open Constantarrays
(* [pow b e] returns the integer result of b raised to the power of e
   Requires: e >= 0 *)
let rec pow b e = match e with
  | 0 -> 1
  | 1 -> b
  | n -> 
    let c = pow b (n / 2) in
    c * c * (if n mod 2 = 0 then 1 else b)

(** [hand_evaluator card_ranks card_suits] returns the integer position of the 5 cards
    cs in the rank array hands 
    hands=["4 of a Kind", "Straight Flush", "Straight", "Flush", "High Card",
       "1 Pair", "2 Pair", "Royal Flush", "3 of a Kind", "Full House" ]; *)
let hand_evaluator card_ranks card_suits =
  let s = (1 lsl card_ranks.(0)) lor (1 lsl card_ranks.(1)) lor (1 lsl card_ranks.(2)) lor (1 lsl card_ranks.(3)) lor (1 lsl card_ranks.(4)) in
  let rec loop i v o = 
    if i = 5 then v
    else if o = 0 then loop (i + 1) (v + 1) (pow 2 (card_ranks.(i)*4))
    else loop (i + 1) (v + o * ( (v / o) land 15) + 1) (pow 2 (card_ranks.(i)*4)) in
  let v = loop 0 0 (pow 2 (card_ranks.(0)*4)) in
  let newv = if s land 1 = 0 || s = 0x403c then v mod 15 - 3
    else v mod 15 - if (s / ( s land (1)) = 31) || (s = 0x403c) then 3 else 1 in
  let multiplier = if s = 0x7c00 then -5 else 1 in
  let boolean = if card_suits.(0) = ((card_suits.(1) lor card_suits.(2) lor card_suits.(3) lor card_suits.(4))) then 1 else 0 in
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
      let new_sum = index2array.(k) in loop (i+1) len new_sum newk in
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
    let () = suit_binary.(a land 0x3) <- suit_binary.(a land 0x3) lor binaries_by_id.(a) in
    let () = suit_binary.(b land 0x3) <- suit_binary.(b land 0x3) lor binaries_by_id.(b) in
    let () = suit_binary.(c land 0x3) <- suit_binary.(c land 0x3) lor binaries_by_id.(c) in
    let () = suit_binary.(d land 0x3) <- suit_binary.(d land 0x3) lor binaries_by_id.(d) in
    let () = suit_binary.(e land 0x3) <- suit_binary.(e land 0x3) lor binaries_by_id.(e) in
    let () = suit_binary.(f land 0x3) <- suit_binary.(f land 0x3) lor binaries_by_id.(f) in
    let () = suit_binary.(g land 0x3) <- suit_binary.(g land 0x3) lor binaries_by_id.(g) in
    let target_index = suits.(suit_hash) - 1 in
    flush.(suit_binary.(target_index));
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


(*
let a = 7*4+0 in
let b = 2*4+0 in
let c = 2*4+3 in
let d = 7*4+1 in
let e = 2*4+2 in
let f = 10*4+0 in
let g = 4*4+0 in

let h = 0 * 4 + 0 in
let i = 7 * 4 +2 in

seven_eval a b c d e f g

EXPECTED: 292

let a = 7*4+0 in
let b = 2*4+0 in
let c = 2*4+3 in
let d = 7*4+1 in
let e = 2*4+2 in
let f = 10*4+0 in
let g = 4*4+0 in

let h = 0 * 4 + 0 in
let i = 7 * 4 +2 in

seven_eval a b c d e h i 

Expected: 236

*)

(*
  int i;
  int suit_hash = 0;
  int suit_binary[4] = {0};
  unsigned char quinary[13] = {0};
  int hash;



  if (suits[suit_hash])
    {
      suit_binary[a & 0x3] |= binaries_by_id[a];
      suit_binary[b & 0x3] |= binaries_by_id[b];
      suit_binary[c & 0x3] |= binaries_by_id[c];
      suit_binary[d & 0x3] |= binaries_by_id[d];
      suit_binary[e & 0x3] |= binaries_by_id[e];
      suit_binary[f & 0x3] |= binaries_by_id[f];
      suit_binary[g & 0x3] |= binaries_by_id[g];

      return flush[suit_binary[suits[suit_hash]-1]];
    }

    quinary[(a >> 2)]++;
   quinary[(b >> 2)]++;
   quinary[(c >> 2)]++;
   quinary[(d >> 2)]++;
   quinary[(e >> 2)]++;
   quinary[(f >> 2)]++;
   quinary[(g >> 2)]++;

   hash = hash_quinary(quinary, 13, 7);

  return noflush7[hash];
  }


  let base_5_hash array len k =
  let array_dp = Array.make 5 (Array.make 14 (Array.make 8 0)) in
  let rec loop i len =
    if i = len then sum
    else
      let sum = 
        int hash_quinary(unsigned char q[], int len, int k)
          {
            int sum = 0;
            int i;

            for (i=0; i<len; i++)
                {
                  sum += dp[q[i]][len-i-1][k];

                  k -= q[i];

                  if (k <= 0)
                      {
                        break;
                      }
                }

                return sum;
          }

          int hash_binary(unsigned char q[], int len, int k)
          {
            int sum = 0;
            int i;

            for (i=0; i<len; i++)
                {
                  if (q[i])
                      {
                        if (len-i-1 >= k)
                            sum += choose[len-i-1][k];

                          k--;

                          if (k == 0)
                              {
                                break;
                              }
                      }
                }

                return sum;
          }
*)