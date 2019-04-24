let declare_action_2p bot hole_cards state iterations = 
  let valid_actions = State.avail_action (State.get_avail_action state) in
  let win_rate = estimate_win_rate_mcs_2p iterations (state.num_players) 
      hole_cards (state.table.board) in
  let can_call = List.mem "call" valid_actions in
  let call_amount = if can_call then calculate_pay_amt state else -1 in
  if bot.money = 0 then 
    ("check", 0)
  else
  if win_rate >= 0.85 then ("raise", bot.money)
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
  else if (List.mem "check" valid_actions) && call_amount = 0 
    then ("check", 0) 
    else ("fold",0)