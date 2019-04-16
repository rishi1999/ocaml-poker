open Deck
open Table
open Player

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
  button : int;
  players_in: int list;
  bet: bet;
  avail_action: string list;
  is_new_round : bool;
}

let get_next_player st =
  let rec helper = function
    | x -> let guess = if x + 1 > st.num_players then 1 else x + 1 in
      if List.mem guess st.players_in then guess else helper (guess) in
  helper st.player_turn

let find_participant st target =
  let rec helper target = function
    | [h] -> h
    | h :: t -> if (Player.id h) = target then h else helper target t in
  helper target (Table.participants (st.table))

let exist lst player = List.exists (fun (x, _) -> x = player) lst


let money_to_pot st amount =
  let og_table = st.table in
  let curr_player = find_participant st st.player_turn in
  let updated_player =
    {
      curr_player with
      money = curr_player.money - amount;
    } in

  let rec helper outlst = function
    | [] -> outlst
    | h::t -> if h.id = st.player_turn then helper (updated_player::outlst) t
      else helper (h::outlst) t in

  let changed_participants = List.rev(helper [] og_table.participants) in

  let changed_table =
    {
      og_table with
      dealer = og_table.dealer + amount;
      participants = changed_participants;
    } in


  let rec bet_paid_helper outlst target bet = function
    | [] -> outlst
    | (player, amount)::t -> if player = target then bet_paid_helper
          ((player, amount + bet)::outlst) target bet t
      else bet_paid_helper ((player, amount)::outlst) target bet t in

  let changed_bet =
    {
      bet_player = st.player_turn;
      bet_amount = if st.bet.bet_amount > amount then st.bet.bet_amount else amount;
      bet_paid_amt = if not (exist st.bet.bet_paid_amt st.player_turn) then ((st.player_turn, amount)::st.bet.bet_paid_amt)
        else bet_paid_helper [] st.player_turn amount st.bet.bet_paid_amt;
    } in

  {
    st with
    table = changed_table;
    player_turn = get_next_player st;
    bet = changed_bet;
    avail_action = ["call"; "raise"; "fold";]
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
    hole_cards = [];
  }

let init_bet =
  {
    bet_player = 0;
    bet_amount = 0;
    bet_paid_amt = [];
  }

let bet_paid_amt st = st.bet.bet_paid_amt

(* WHAT IS THE POINT OF THIS FUNCTION? -- removed the sort since the elements already seem to be ordered *)
let init_players_in num_players =
  let rec init_players_in' acc = function
    | 0 -> acc
    | t -> init_players_in' (t :: acc) (t - 1) in
  (*List.sort compare*) (init_players_in' [] num_players)

let init_state game_type num_players money blind =
  (* pay_blinds *)
  {
    game_type;
    num_players;
    table = init_table num_players money blind;
    player_turn = 1;
    button = 1;
    players_in = init_players_in num_players;
    bet = init_bet;
    avail_action = ["fold"; "bet"; "check"];
    is_new_round = true;
  }

let game_type st = st.game_type
let num_players st = st.num_players
let table st = st.table
let player_turn st = st.player_turn
let button st = st.button
let players_in st = st.players_in
let bet st = st.bet
let avail_action st = st.avail_action
let is_new_round st = st.is_new_round

let go_next_round st =
  if List.length st.table.hole_cards = 5 then
    let cleared = Table.clear_round st.table in
    {
      st with
      table = Table.deal (cleared);
    }
  else
    let card_added = Table.add_to_hole st.table in
    {
      st with
      table = card_added;
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

(* (** [are_all_bets_equal] is true if all bets made
    in the current round are equal. *)
   let are_all_bets_equal st = List.for_all
    (fun (player,paid) -> paid = st.bet.bet_amount) st.bet.bet_paid_amt *)

(* TODO WHAT IS THIS FUNCTION?? *)
let get_avail_action st = st

(** [is_round_complete st] is true if the game is
    ready to move on to the next round. *)
let is_round_complete st =
  let rec bets_helper = function
    | [] -> true
    | (p, amt)::t ->
      if List.mem p st.players_in then
        if amt = st.bet.bet_amount then bets_helper t
        else false
      else bets_helper t in

  (st.is_new_round && bets_helper st.bet.bet_paid_amt) ||
  List.length st.players_in = 0

let calculate_pay_amt st =
  let cur_bet_size = st.bet.bet_amount in
  let rec get_bet_amt target = function
    | [] -> 0
    | (p, a)::t -> if p = target then a else get_bet_amt target t in

  if exist st.bet.bet_paid_amt st.player_turn then
    Pervasives.abs(cur_bet_size - get_bet_amt st.player_turn st.bet.bet_paid_amt)
  else
    cur_bet_size

type move_result =
  | Legal of t
  | Illegal

let check st =
  if List.mem "check" st.avail_action then
    if is_round_complete st then
      Legal (go_next_round st)
    else
      Legal
        {
          st with
          player_turn = get_next_player st
        }
  else Illegal

let call st =
  if List.mem "call" st.avail_action then
    let t = money_to_pot st (calculate_pay_amt st) in
    if is_round_complete t then
      Legal (go_next_round t)
    else
      Legal t
  else Illegal

let fold st =
  if List.mem "fold" st.avail_action then
    let remove target lst = List.filter (fun x -> not (x = target)) lst in
    let t =
      {
        st with
        players_in = remove st.player_turn st.players_in;
        player_turn = get_next_player st;
      } in

    if is_round_complete t then
      Legal (go_next_round t)
    else
      Legal t
  else Illegal

let stack st =
  if List.mem "stack" st.avail_action then
    let players = List.sort compare st.players_in in
    let rec find_stack player = function
      | [] -> 0
      | h::t -> if h.id = player then h.money else find_stack player t in
    let print_stack player =
      print_string "Player ";
      print_int player;
      print_string " has $";
      print_int (find_stack player st.table.participants);
      print_endline ". "; in
    List.iter print_stack players;
    Legal st
  else Illegal

let do_the_money amt st comm_str =
  if List.mem comm_str st.avail_action then
    let t = money_to_pot st amt in
    let minimum_amount = 234 in (* TODO WHAT IS THE MINIMUM BET AMOUNT *)
    (* TODO ALSO WE PROBABLY NEED TO MAKE SURE THAT THEY'RE EXCEEDING
       THE CURRENT BET AMOUNT RIGHT? *)
    if amt >= minimum_amount then
      Legal t
    else Illegal
  else Illegal

let bet' amt st = do_the_money amt st "bet"
let raise' amt st = do_the_money amt st "raise"

let command_to_function = Command.(function
    | Check -> check
    | Bet amt -> bet' amt
    | Call -> call
    | Raise amt -> raise' amt
    | Fold -> fold
    | Stack -> stack
    | _ -> failwith "UNSUPPORTED COMMAND"
  )
