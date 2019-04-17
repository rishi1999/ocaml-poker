open Deck
open Table
open Player
open Hand_evaluator

(** [bet] is the bet situation of the current round:
    [bet_player] : the player that has bet / raised the last
    [bet_amount] : the current bet amount that the next player has to match
    [bet_paid_amt] : the current bet situation in form (player, bet_amount) list
*)
type bet = {
  bet_player: int;
  bet_amount: int;
  bet_paid_amt: (int*int) list;
}

(** [t] is the state of the game described using the following information:
    [game_type] : an integer representin a game type
      0 if it is a multiplayer game, 1 if it is against the AI
    [num_players] : the number of players in the game
    [table] : type Table.table that represents the table
    [player_turn] : the player that has the action
    [button] : the person that goes last in the hand
    [players_in] : the list of players that are currently playing the hand
    [bet] : current bet situation in this round
    [avail_action] : the available actions that the current player can act
*)
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
}

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

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
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

let has_everyone_played st = List.sort compare st.players_in = List.sort compare st.players_played

(** [is_round_complete st] is true if the game is
    ready to move on to the next round. *)
let is_round_complete st =
  are_all_bets_equal st &&
  has_everyone_played st


(** [is_hand_complete st] is true if hand is complete. *)
let is_hand_complete st =
  let everyone_folded = (List.length st.players_in < 2) in
  let after_river = (List.length st.table.board = 5) in

  everyone_folded || (after_river && is_round_complete st)

let rec get_players_in part players_in ls = match players_in with
  | a::b -> (List.nth part (a-1)) :: ls
  | [] -> ls

let winner st =
  let board = match st with
    | {
      game_type;
      num_players;
      table = t;
      player_turn;
      button;
      players_in;
      bet;
      avail_action;
    } -> t.board
  in
  let all_part = match st with
    | {
      game_type;
      num_players;
      table = t;
      player_turn;
      button;
      players_in;
      bet;
      avail_action;
    } -> t.participants
  in

  let p_in = match st with
    | {
      game_type;
      num_players;
      table;
      player_turn;
      button;
      players_in = ls;
      bet;
      avail_action;
    } -> ls
  in

  (** ranks returns a list of ranks of the hands of the list players*)
  let rec ranks participants (board : Deck.card list) lst =
    match participants with
    | a::b -> ranks b board ((seven_list_eval (a.cards @ board))::lst)
    | [] -> lst
  in

  (** best_rank gets the best rank in the list of hands*)
  let rec best_rank (ls: int list) (acc: int) = match ls with
    | [] -> acc
    | a::b when a<acc -> best_rank b a
    | a::b when a>acc -> best_rank b acc
    | _ -> failwith "cannot find best"
  in

  (** [get_player_in target ls acc] is the integer position
      of the list of the best player. *)
  let rec get_player_int (target:int) ls acc = match ls with
    | a::b when a = target -> acc
    | a::b -> get_player_int target b (acc+1)
    | [] -> failwith "not in list" in
  let part = get_players_in all_part p_in [] in
  let rlist = ranks part board [] in
  let num_winner = get_player_int (best_rank rlist 0) rlist 0 in
  List.nth part num_winner

let go_next_round st =
  if is_hand_complete st then
    (* everyone folded *)
    (* let winner_player = if List.length st.players_in = 1 then List.hd st.players_in else (winner st).id in
       let win_amount = st.table.dealer in
       let player_won = find_participant st winner_player in
       let player_paid = {player_won with money = player_won.money + win_amount} in

       let rec update_parcipant target player outlst = function
       | [] -> outlst
       | h::t -> if h.id = target then update_parcipant target player (player::outlst) t
        else update_parcipant target player (h::outlst) t in

       let updated_participants = update_parcipant winner_player player_paid [] st.table.participants in

       let updated_table = {
       st.table with
       participants = updated_participants;
       } in *)

    (* let cleared = Table.clear_round updated_table in *)
    let cleared = Table.clear_round st.table in
    let button_updated = if st.button + 1 > st.num_players then 1 else st.button + 1 in
    let players_in_updated = hand_order st.num_players button_updated in
    {
      st with
      table = Table.deal (cleared);
      bet = init_bet players_in_updated;
      player_turn = List.nth players_in_updated 0;
      button = button_updated;
      players_in = players_in_updated;
      players_played = [];
    } |> pay_blinds
  else
    let card_added = Table.add_to_hole st.table in
    {
      st with
      table = card_added;
      bet = init_bet st.players_in;
      player_turn = List.nth st.players_in 0;
      players_played = [];
    }

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
      avail_action = ["check"; "bet"; "fold"; "stack"]
    }
  else
    {
      st with
      avail_action = ["call"; "raise"; "fold"; "stack"]
    }

let calculate_pay_amt st =
  let cur_bet_size = st.bet.bet_amount in
  let rec get_bet_amt target = function
    | [] -> 0
    | (p, a)::t -> if p = target then a else get_bet_amt target t in

  Pervasives.abs(cur_bet_size - get_bet_amt st.player_turn st.bet.bet_paid_amt)


type move_result =
  | Legal of t
  | Illegal

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
  else Illegal

let call st =
  if List.mem "call" st.avail_action then
    let t = money_to_pot st (calculate_pay_amt st) in
    if is_round_complete t || is_hand_complete t then
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
        bet = st.bet;
      } in

    if is_round_complete t || is_hand_complete t then
      Legal (go_next_round t)
    else
      Legal t
  else Illegal

let stack st =
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
  List.iter print_stack (List.sort compare players);
  Legal st

let bet_or_raise amt st comm_str =
  if List.mem comm_str st.avail_action then
    if amt >= st.table.blind then Legal (money_to_pot st amt)
    else Illegal
  else Illegal

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
