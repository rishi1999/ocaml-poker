open Deck
open Hand_evaluator
open State
open Player

(* [community_card_fill base_cards used] returns a random number of  *)
let community_card_fill base_cards used = 
  let number = 5 - List.length (base_cards) in
  let required_cards = pick_new number used in
  base_cards @ required_cards

(* PyPoker Engine stuff *)
let montecarlo_simulation num_players hole board =
  let used = hole @ board in
  let community_card = community_card_fill board used in
  let unused_card = pick_new ((num_players - 1) * 2) used in 
  let rec hand_builder current_hand count hand_list card_list = match card_list with
    |  [] -> List.rev hand_list
    | h :: t  when count = 2 -> hand_builder [] 0 (current_hand :: hand_list) t
    | h :: t -> hand_builder (h :: current_hand) (count + 1) hand_list t in
  let opponent_hands = hand_builder [] 0 [] unused_card in
  let rec opponent_rank_builder outlist hand_list = match hand_list with
    | [] -> List.rev outlist
    | h :: t -> let hand_score = seven_list_eval (h @ community_card) in
      opponent_rank_builder (hand_score :: outlist) t in
  let opponent_rank = opponent_rank_builder [] opponent_hands in
  let min_opponent_rank = List.fold_left (fun accu elem -> min accu elem) 7463 opponent_rank in
  let my_score = seven_list_eval (hole @  community_card) in
  if my_score < min_opponent_rank then 1.0 else 0.0

(* estimate_win_rate *)
let estimate_win_rate num_simulations num_player hole board =
  let rec win_count sum count = 
    if count = 0 then sum 
    else let win = montecarlo_simulation num_player hole board in 
      win_count (sum +. win) (count - 1) in
  let wins = win_count 0.0 num_simulations in
  let float_simulations = float_of_int num_simulations in
  wins /. float_simulations

let declare_action bot hole_cards state iterations = 
  let valid_actions = state.avail_action in
  let win_rate = estimate_win_rate iterations (state.num_players) hole_cards (state.table.board) in
  let can_call = List.mem "call" valid_actions in
  let call_amount = if can_call then calculate_pay_amt state else 0 in
  if bot.money = 0 then 
    ("check", 0)
  else
    if win_rate >= 0.85 then ("raise", bot.money)
    else if win_rate > 0.75 then ("raise", 2 * state.bet.bet_amount)
    else if win_rate > 0.5 then ("call" , call_amount)
    else if can_call && call_amount = 0 then ("check", 0) else ("fold",0)