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
  new_round: bool;
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

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
let exist lst player = List.exists (fun (x, _) -> x = player) lst

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
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
      new_round = false;
    } in

  {
    st with
    table = changed_table;
    player_turn = get_next_player st;
    bet = changed_bet;
    avail_action = ["call"; "raise"; "fold";]
  }

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
let pay_blinds st =
  let small_blind = money_to_pot st (st.table.blind / 2) in
  money_to_pot small_blind st.table.blind

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
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

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
let init_table num_players money blind =
  Table.deal {
    dealer = 0;
    blind;
    participants = init_players num_players money;
    board = [];
  }

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
let init_bet =
  {
    bet_player = 0;
    bet_amount = 0;
    bet_paid_amt = [];
    new_round = true;
  }

let init_players_in num_players =
  let rec init_players_in' acc = function
    | 0 -> acc
    | t -> init_players_in' (t :: acc) (t - 1) in
  init_players_in' [] num_players

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

(** [list_remove_element] returns a list with all the elements of [list]
    except for [element].
    Example: [list_remove_element] 1 [1;2;3] is [2;3]. *)
let init_state game_type num_players money blind =
  {
    game_type;
    num_players;
    table = init_table num_players money blind;
    player_turn = 1;
    button = num_players;
    players_in = init_players_in num_players;
    bet = init_bet;
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

(* (** [are_all_bets_equal] is true if all bets made
    in the current round are equal. *)
   let are_all_bets_equal st = List.for_all
    (fun (player,paid) -> paid = st.bet.bet_amount) st.bet.bet_paid_amt *)
let check_all_bet_equal st =
  let rec players_all_paid = function
  | [] -> true
  | h :: t -> if exist st.bet.bet_paid_amt h then players_all_paid t
    else false in
  
  players_all_paid st.players_in && are_all_bets_equal st

(** [is_round_complete st] is true if the game is
    ready to move on to the next round. *)
let is_round_complete st =
  let everyone_checked = (st.bet.bet_amount = 0 && st.bet.new_round = false &&
                          st.player_turn = List.nth st.players_in 0) in
  (* NEEDS WORK *)
  if List.length st.table.board = 0 then
    let big_blind = List.nth st.players_in 1 in
    let bb = (st.player_turn = big_blind) in
    check_all_bet_equal st
  else
    everyone_checked || (check_all_bet_equal st && st.bet.new_round = false
    && st.bet.bet_amount != 0)

(** [is_hand_complete st] is true if hand is complete. *)
let is_hand_complete st =
  let everyone_checked = (st.bet.bet_amount = 0 && st.bet.new_round = false &&
                          st.player_turn = List.nth st.players_in 0) in
  let everyone_folded = (List.length st.players_in < 2) in
  let after_river = (List.length st.table.board = 5) in

  everyone_folded || (after_river && check_all_bet_equal st
                      && st.bet.new_round = false)
                      || (after_river && everyone_checked)

let rec get_players_in part players_in ls = match players_in with
  | a::b -> (List.nth part (a-1)) :: ls
  | [] -> ls

let winner st =
  let hole = match st with
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
  let rec ranks participants (board : Deck.card list) lst = match participants with
    | a::b -> print_int (seven_list_eval (a.cards@hole)); print_string ("yuh"); ranks b board ((seven_list_eval (a.cards@hole))::lst)
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
  let rlist = ranks part hole [] in
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
    {
      st with
      table = Table.deal (cleared);
      bet = init_bet;
      player_turn = List.nth st.players_in 0;
      button = button_updated;
      players_in = hand_order st.num_players button_updated;
    }
  else
    let card_added = Table.add_to_hole st.table in
    {
      st with
      table = card_added;
      bet = init_bet;
      player_turn = List.nth st.players_in 0;
    }

(* need to call get_avail_action before each turn to get the proper actions*)
let get_avail_action st =
  (* preflop *)
  if List.length st.table.board = 0 then
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
  else
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

  if exist st.bet.bet_paid_amt st.player_turn then
    Pervasives.abs(cur_bet_size - get_bet_amt st.player_turn st.bet.bet_paid_amt)
  else
    cur_bet_size

type move_result =
  | Legal of t
  | Illegal

let check st =
  if List.mem "check" st.avail_action then
    let checked = {
          st with
          player_turn = get_next_player st;
          bet = {st.bet with 
            new_round = false;
          }} in
    if is_round_complete checked then
      Legal (go_next_round checked)
    else
      Legal
        checked
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
        bet = {st.bet with
               new_round = false;
              };
      } in

    if is_round_complete t then
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
  List.iter print_stack players;
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


