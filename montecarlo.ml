open Deck
open Hand_evaluator
open State
open Player


(** [community_card_fill_eff base_cards used] returns a random number of cards
    from the deck but does not return any card that is present in [used]
    The number of cards returned is equal to 5 - the number of elements
    in [base_cards].
    Example: [community_card_fill_eff [1;2;3;5] [1;2;3;4;6;7]] could be
    [4].
    Requires: [base_cards] and [used_cards] to be lists containing integers
    from 0 (inclusive) to 51 (inclusive). All elements of [base_cards]
    must be inside [used_cards]. *)
let community_card_fill_eff base_cards used =
  let number_needed = 5 - List.length (base_cards) in
  let required_cards = pick_efficient number_needed used in
  base_cards @ required_cards

(** [montecarlo_simulation_efficient num_players hole board] runs
    a montecarlo simulation for a bot with current hand [hole] and
    visible community cards [board]. [num_players - 1] hands are simulated to
    represent the hands of the players other than the bot that are in the
    game. If [hole] and [board] form a better hand than the hands randomly
    chosen for the other players with the same [board], the functions
    evaluates to 1.0, otherwise returns 0.0.
    If [board] contains less than 5 integers, the remaining integers are
    also randomly simulated.
    Example: [montecarlo_simulation_efficient 2 [1;2] [3;4;5;6;7]]
    could be 1. The exact result could be either 1.0 or 0.0 depending
    on the random opponent hands that are simulated.
    Requires: [hole] is an int list containing 2 cards represented as integers
    from 0 (inclusive) to 51 (inclusive), the integers in [hole]
    and those in [board] must be distinct from each other and [num_players]
    must be a positive value. *)
let montecarlo_simulation_efficient num_players hole board =
  let community_card = community_card_fill_eff board (hole @ board) in
  let unused_card = pick_efficient ((num_players - 1) * 2)
      (hole @ community_card) in
  let rec hand_builder current_hand count hand_list card_list =
    match card_list with
    |  [] -> List.rev hand_list
    | h :: t  when count = 1 -> let new_hand = h :: current_hand in
      hand_builder [] 0 (new_hand :: hand_list) t
    | h :: t -> hand_builder (h :: current_hand) (count + 1) hand_list t in
  let opponent_hands = hand_builder [] 0 [] unused_card in
  let rec opponent_rank_builder outlist hand_list = match hand_list with
    | [] -> List.rev outlist
    | h :: t -> let hand_score = seven_int_list_eval (h @ community_card) in
      opponent_rank_builder (hand_score :: outlist) t in
  let opponent_rank = opponent_rank_builder [] opponent_hands in
  let min_opponent_rank = List.fold_left (fun accu elem -> min accu elem)
      7463 opponent_rank in
  let my_score = seven_int_list_eval (hole @  community_card) in
  if my_score < min_opponent_rank then 1.0 else 0.0

(** [estimate_win_rate_eff num_simulations num_player hole board] runs
    [num_simulations] montecarlo simulation for a bot with current hand [hole]
    and visible community cards [board]. [num_players - 1] hands are simulated 
    to represent the hands of the players other than the bot that are in the
    game. Evaluates to the ratio of the number of times the [hole] and [board]
    form a better hand than the hands randomly chosen for the other players 
    with the same [board]. Each simulation randomly selects additional 
    community cards if [board] contains less than 5 integers. 
    Example: [estimate_win_rate_eff 10000 [1;2] [3;4;5;6;7]]
    could be 0.78. The exact result varies as the cards are randomly selected
    and the seed values are randomly initialized.
    Requires: [hole] is an int list containing 2 cards represented as integers
    from 0 (inclusive) to 51 (inclusive), the integers in [hole]
    and those in [board] must be distinct from each other and [num_players]
    must be a positive value. *)
let estimate_win_rate_eff num_simulations num_player hole board =
  let int_hole = List.map int_converter hole in
  let int_board = List.map int_converter board in
  let rec win_count sum count = 
    if count = 0 then sum 
    else let win = montecarlo_simulation_efficient num_player int_hole 
             int_board in 
      win_count (sum +. win) (count - 1) in
  let wins = win_count 0.0 num_simulations in
  let float_simulations = float_of_int num_simulations in
  wins /. float_simulations

(** [declare_action_eff bot hole_cards state iterations] runs 
    [iterations] montecarlo simulations and then returns a string * amount
    tuple to represent the action that the bot should take.
    Example: [declare_action_eff [;money = 25;...] [1;2] [...;avail_actions = 
    ["call";"raise";"fold"];...;num_players = 2; table.board = [3;4;5;6;7]...]
    10000] could be [raise, 50]. The exact output depends on the
    random simulations that are run and what the results of those simulations
    is.
    Requires: [bot] is a valid player of type Player.player, [hole_cards] is 
    an int list containing 2 cards represented as integers from 0 (inclusive) 
    to 51 (inclusive), [iterations] is a positive value and [state] is
    a valid record of type State.t *)
let declare_action_eff bot hole_cards state iterations = 
  let valid_actions = state.avail_action in
  let win_rate = estimate_win_rate_eff iterations (state.num_players) 
      hole_cards (state.table.board) in
  let can_call = List.mem "call" valid_actions in
  let call_amount = if can_call then calculate_pay_amt state else 0 in
  if bot.money = 0 then 
    ("check", 0)
  else
  if win_rate >= 0.85 then ("raise", bot.money)
  else if win_rate > 0.75 then ("raise", 2 * state.bet.bet_amount)
  else if win_rate > 0.5 then ("call" , call_amount)
  else if can_call && call_amount = 0 then ("check", 0) else ("fold",0)

(** [mcs_2p hole board] runs
    a montecarlo simulation for a bot with current hand [hole] and
    visible community cards [board]. 1 hand is simulated for the other
    player in the game. If [hole] and [board] form a better hand than the 
    hand randomly chosen for the other player with the same [board], 
    the function evaluates to 1.0, otherwise returns 0.0.
    Example: [montecarlo_simulation_efficient 2 [1;2] [3;4;5;6;7]]
    could be 1. The exact result could be either 1.0 or 0.0 depending
    on the random opponent hands that are simulated.
    Requires: [hole] is an int list containing 2 cards represented as integers
    from 0 (inclusive) to 51 (inclusive), the integers in [hole]
    and those in [board] must be distinct from each other and [num_players]
    must be a positive value. *)
let mcs_2p hole board =
  let community_card = community_card_fill_eff board (hole @ board) in
  let unused_card = pick_efficient 2 (hole @ community_card) in
  let opponent_score = seven_int_list_eval (unused_card @ community_card) in
  let my_score = seven_int_list_eval (hole @  community_card) in
  if my_score < opponent_score then 1.0 else 0.0

(** [estimate_win_rate_mcs_2p num_simulations hole board] runs
    [num_simulations] montecarlo simulation for a bot with current hand [hole] 
    and visible community cards [board]. 1 hand is simulated 
    to represent the hands of the players other than the bot that are in the
    game. Evaluates to the ratio of the number of times the [hole] and [board]
    form a better hand than the hand randomly chosen for the other player 
    with the same [board]. Each simulation randomly selects additional 
    community cards if [board] contains less than 5 integers. 
    Example: [estimate_win_rate_eff 10000 [1;2] [3;4;5;6;7]]
    could be 0.78. The exact result varies as the cards are randomly selected
    and the seed values are randomly initialized.
    Requires: [hole] is an int list containing 2 cards represented as integers
    from 0 (inclusive) to 51 (inclusive), the integers in [hole]
    and those in [board] must be distinct from each other. *)
let estimate_win_rate_mcs_2p num_simulations hole board =
  let int_hole = List.map int_converter hole in
  let int_board = List.map int_converter board in
  let rec win_count sum count = 
    if count = 0 then sum 
    else let win = mcs_2p int_hole int_board in 
      win_count (sum +. win) (count - 1) in
  let wins = win_count 0.0 num_simulations in
  let float_simulations = float_of_int num_simulations in
  wins /. float_simulations

(** [declare_action_2p bot hole_cards state iterations] runs 
    [iterations] montecarlo simulations and then returns a string * amount
    tuple to represent the action that the bot should take when playing
    against one other human player.
    Example: [declare_action_eff [;money = 25;...] [1;2] [...;avail_actions = 
    ["call";"raise";"fold"];...table.board = [3;4;5;6;7]...] 
    10000] could be [raise, 50]. The exact output depends on the
    random simulations that are run and what the results of those simulations
    is.
    Requires: [bot] is a valid player of type Player.player, [hole_cards] is 
    an int list containing 2 cards represented as integers from 0 (inclusive) 
    to 51 (inclusive), [iterations] is a positive value and [state] is
    a valid record of type State.t *)
let declare_action_2p bot hole_cards state iterations = 
  let valid_actions = State.avail_action (State.get_avail_action state) in
  let win_rate = estimate_win_rate_mcs_2p iterations 
      hole_cards (state.table.board) in
  let can_call = List.mem "call" valid_actions in
  let call_amount = if can_call then calculate_pay_amt state else -1 in
  if bot.money = 0 then
    ("check", 0)
  else
  if win_rate >= 0.85 then
    if List.mem "bet" valid_actions then
      ("bet", bot.money/2)
    else
      ("raise", bot.money)
  else if win_rate > 0.75 then
    if List.mem "bet" valid_actions then
      ("bet", bot.money/5)
    else
      ("raise", (Pervasives.min (2 * state.bet.bet_amount) bot.money))
  else if win_rate > 0.5 then
    if call_amount = -1 then
      ("check", 0)
    else
      ("call" , call_amount)
  else if win_rate > 0.3 then
    if call_amount = -1 then
      ("check", 0)
    else
    if (call_amount < bot.money / 30) then
      ("call", call_amount)
    else
      ("fold", 0)
  else if (List.mem "check" valid_actions) && call_amount = -1
  then ("check", 0)
  else ("fold",0)