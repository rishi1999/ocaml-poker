open Deck
open Table
open Player
open Hand_evaluator

type bet = {
  bet_player: int;
  bet_amount: int;
  bet_paid_amt: (int*int) list;
}

type t = {
  game_type: int;
  num_players: int;
  table: Table.table;
  player_turn: int;
  button: int;
  players_in: int list;
  players_played: int list;
  bet: bet;
  avail_action: string list;
  winners : int list;
}
exception Tie

(** [get_next_player] st returns the number of the player that has
    to act next. *)
let get_next_player st =
  let rec helper = function
    | x -> let guess = if x + 1 > st.num_players then 1 else x + 1 in
      if List.mem guess st.players_in then guess else helper (guess) in
  helper st.player_turn

(** [find_participant] st target returns a type Player.player of a player that
    has an id of target. *)
let find_participant st target =
  let rec helper target = function
    | [] -> failwith "No match"
    | [h] -> h
    | h :: t -> if (Player.id h) = target then h else helper target t in
  helper target (Table.participants (st.table))

let money_to_pot st amount =
  let player = find_participant st st.player_turn in
  let player' =
    {
      player with
      money = player.money - amount;
    } in

  let rec helper outlst = function
    | [] -> outlst
    | h::t -> if h.id = st.player_turn then helper (player'::outlst) t
      else helper (h::outlst) t in

  let participants' = List.rev(helper [] st.table.participants) in

  let table =
    {
      st.table with
      dealer = st.table.dealer + amount;
      participants = participants';
    } in

  let rec update_bet_paid acc target bet = function
    | [] -> acc
    | (pl, money) :: t ->
      let x =
        if pl = target
        then (pl, money + bet)
        else (pl, money) in
      update_bet_paid (x :: acc) target bet t in

  let bet' =
    {
      bet_player = st.player_turn;
      bet_amount = if st.bet.bet_amount > amount then st.bet.bet_amount
        else amount;
      bet_paid_amt = update_bet_paid
          [] st.player_turn amount st.bet.bet_paid_amt;
    } in

  {
    st with
    table;
    player_turn = get_next_player st;
    bet = bet';
    avail_action = ["call"; "raise"; "fold";];
    players_played = st.player_turn :: st.players_played;
  }

let pay_blinds st =
  let small_blind = money_to_pot st (st.table.blind / 2) in
  money_to_pot small_blind st.table.blind

let init_players num_players money =
  let rec init_players' acc money = function
    | 0 -> acc
    | id -> let curr_player =
              {
                id;
                cards = [];
                money;
              } in
      init_players' (curr_player :: acc) money (id - 1) in
  init_players' [] money num_players

let init_table num_players money blind =
  Table.deal {
    dealer = 0;
    blind;
    participants = init_players num_players money;
    board = [];
  }

let init_bet_paid_amt players_in =
  let rec helper lst = function
    | [] -> lst
    | h::t -> helper ((h,0)::lst) t in
  helper [] players_in

let init_players_in num_players =
  let rec init_players_in' acc = function
    | 0 -> acc
    | t -> init_players_in' (t :: acc) (t - 1) in
  init_players_in' [] num_players

let init_bet players_in =
  {
    bet_player = 0;
    bet_amount = 0;
    bet_paid_amt = init_bet_paid_amt players_in;
  }

(** [hand_order num_players button] is an integer list
    containing integers from (button + 1) to num_players and then from 1
    to button.
    Requires: button >= 1 and num_players >=1
    Requires: button <= num_players
    Example:  [hand_order 5 3] is [4; 5; 1; 2; 3] *)
let hand_order num_players button =
  let rec list_builder start term outlist =
    if start > term then outlist
    else list_builder start (term - 1) (term :: outlist) in
  let second = list_builder 1 button [] in
  let first = list_builder (button + 1) num_players [] in
  first @ second

let init_state game_type num_players money blind =
  {
    game_type;
    num_players;
    table = init_table num_players money blind;
    player_turn = 1;
    button = num_players;
    players_in = init_players_in num_players;
    players_played = [];
    bet = init_bet (init_players_in num_players);
    avail_action = ["bet"; "check"; "fold"];
    winners = [-1];
  } |> pay_blinds

let game_type st = st.game_type
let num_players st = st.num_players
let table st = st.table
let player_turn st = st.player_turn
let button st = st.button
let players_in st = st.players_in
let bet st = st.bet
let avail_action st = st.avail_action
let bet_paid_amt st = st.bet.bet_paid_amt


(** [are_all_bets_equal] is true if all bets made
    in the current round are equal. *)
let are_all_bets_equal st = List.for_all
    (fun (_,paid) -> paid = st.bet.bet_amount) st.bet.bet_paid_amt

let has_everyone_played st =
  let rec check_subset set subset =
    match subset with
    | [] -> true
    | h::t -> if List.mem h set then check_subset set t
      else false in
  check_subset st.players_played st.players_in

(* let print_that_list lst = List.iter (fun x -> print_int x) lst *)

(** [is_round_complete st] is true if the game is
    ready to move on to the next round. *)
let is_round_complete st =
  (*print_that_list st.players_in; print_newline (); print_that_list st.players_played;*)
  are_all_bets_equal st &&
  has_everyone_played st


(** [is_hand_complete st] is true if hand is complete. *)
let is_hand_complete st =
  let everyone_folded = (List.length st.players_in < 2) in
  let after_river = (List.length st.table.board = 5) in

  everyone_folded || after_river && is_round_complete st

let rec get_players_in part players_in ls = match players_in with
  | [] -> List.rev ls
  | a :: t when List.mem a.id part -> get_players_in part t (a :: ls)
  | a :: t -> get_players_in part t ls

let winners st =
  let compare_tuples (x1,_) (y1,_) = x1 - y1 in
  let rec hand_ranks (brd : Deck.card list) acc = function
    | [] -> List.sort compare_tuples acc
    | p :: t -> hand_ranks brd ((seven_list_eval (p.cards @ brd), p) :: acc) t in

  let winning_hands lst =
    let winning_hand_rank = List.hd lst in
    let rec winning_hands' acc = function
      | h :: t when h = winning_hand_rank -> winning_hands' (h :: acc) t
      | _ -> acc in
    List.rev (winning_hands' [] lst) in

  winning_hands (hand_ranks st.table.board [] st.table.participants) |>

  List.map (fun (_,p) -> p) |>

  List.filter (fun x ->
      not (List.for_all (fun y -> not (x.id = y)) st.players_in))

let go_next_round st =
  if is_hand_complete st then
    let players_won = winners st in
    let winner_ids = List.map (fun x -> x.id) players_won in
    let win_amount = st.table.dealer in
    let players_paid = List.map (fun x ->
        {
          x with
          money = x.money + win_amount
        }
      ) players_won
    in
    let rec update_participant target player outlst = function
      | [] -> outlst
      | h::t -> if h.id = target then
          update_participant target player (player::outlst) t
        else update_participant target player (h::outlst) t in
    let updated_participants = List.fold_left2
        (fun a b c -> update_participant b c [] a)
        st.table.participants winner_ids players_paid in
    let updated_table = {
      st.table with
      participants = updated_participants;
    } in
    let cleared = Table.clear_round updated_table in
    let button_updated = if st.button + 1 > st.num_players then
        1
      else st.button + 1 in
    let players_in_updated = hand_order st.num_players button_updated in
    let interim_state = {
      st with
      table = Table.deal (cleared);
      bet = init_bet players_in_updated;
      player_turn = List.nth players_in_updated 0;
      button = button_updated;
      players_in = players_in_updated;
      players_played = [];
      winners = winner_ids;
    } in
    let updated_state = pay_blinds interim_state in updated_state
  else
    let card_added = Table.add_to_hole st.table in
    {
      st with
      table = card_added;
      bet = init_bet st.players_in;
      player_turn = List.nth st.players_in 0;
      players_played = [];
    }

let continue_game st = {st with winners = [-1]}

let winning_players st = st.winners

(* need to call get_avail_action before each turn to get the proper actions*)
let get_avail_action st =
  (* preflop *)
  (* if List.length st.table.board = 0 then
     let big_blind_player = List.nth st.players_in 1 in
     if st.player_turn = big_blind_player && st.bet.bet_amount = st.table.blind then
      {
        st with
        avail_action = ["check"; "bet"; "fold"];
      }
     else
      {
        st with
        avail_action = ["call"; "raise"; "fold"];
      }
      (* flop *)
     else *)
  if st.bet.bet_amount = 0 then
    {
      st with
      avail_action = ["check"; "bet"; "fold"]
    }
  else
    {
      st with
      avail_action = ["call"; "raise"; "fold"]
    }

let calculate_pay_amt st =
  let cur_bet_size = st.bet.bet_amount in
  let rec get_bet_amt target = function
    | [] -> 0
    | (p, a)::t -> if p = target then a else get_bet_amt target t in

  Pervasives.abs(cur_bet_size - get_bet_amt st.player_turn st.bet.bet_paid_amt)

let rec find_stack player = function
  | [] -> 0
  | h::t -> if h.id = player then h.money else find_stack player t

type move_result =
  | Legal of t
  | Illegal of string

let check st =
  if List.mem "check" st.avail_action then
    let checked = {
      st with
      player_turn = get_next_player st;
      players_played = st.player_turn :: st.players_played;
      bet = st.bet;
    } in
    if is_round_complete checked || is_hand_complete checked then
      Legal (go_next_round checked)
    else
      Legal
        checked
  else Illegal "You can't do that right now!"

let call st =
  if List.mem "call" st.avail_action then
    if calculate_pay_amt st <=
       (find_stack st.player_turn st.table.participants) then
      let t = money_to_pot st (calculate_pay_amt st) in
      if is_round_complete t || is_hand_complete t then
        Legal (go_next_round t)
      else
        Legal t
    else Illegal "You are out of money!"
  else Illegal "You can't do that right now!"

let fold st =
  if List.mem "fold" st.avail_action then
    let remove target lst = List.filter (fun x -> not (x = target)) lst in
    let pl = st.player_turn in
    let t =
      {
        st with
        players_in = remove pl st.players_in;
        player_turn = get_next_player st;
        bet =
          {
            st.bet with
            bet_paid_amt = (List.remove_assoc pl st.bet.bet_paid_amt);
          };
      } in

    if is_round_complete t || is_hand_complete t then
      Legal (go_next_round t)
    else
      Legal t
  else Illegal "You can't do that right now!"

let stack st =
  print_newline ();
  let players = List.sort compare st.players_in in
  let print_stack player =
    print_string "Player ";
    print_int player;
    print_string " has $";
    print_int (find_stack player st.table.participants);
    print_endline ". "; in
  List.iter print_stack (List.sort compare players);
  Legal st

let bet_or_raise amt st comm_str =
  if List.mem comm_str st.avail_action then
    if amt >= st.table.blind &&
       amt <= (find_stack st.player_turn st.table.participants) then
      Legal (money_to_pot st amt)
    else Illegal "You are out of money!"
  else Illegal "You can't do that right now!"

let bet' amt st = bet_or_raise amt st "bet"
let raise' amt st = bet_or_raise amt st "raise"

let command_to_function = Command.(function
    | Check -> check
    | Bet amt -> bet' amt
    | Call -> call
    | Raise amt -> raise' amt
    | Fold -> fold
    | Stack -> stack
    | _ -> failwith "UNSUPPORTED COMMAND"
  )
