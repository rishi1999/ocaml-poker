open Deck
open Table
open Player

type bet = {
  bet_player: int;
  bet_amount: int;
  bet_paid_amt: int list;
}

type t = {
  game_type: int;
  player_number: int;
  table: Table.table;
  player_turn: int;
  button : int;
  (* players playing *)
  players_in: int list;
  (* who bet? *)
  bet: bet;
  avail_action: string list;
}

let init_player num_players money =
  let cnt = ref 0 in
  let rec init_player_helper outlst money = function
    | 0 -> outlst
    | t -> let curr_player = {id = !cnt; action = None; cards = []; money = money} in
      cnt := !cnt + 1;
      init_player_helper (curr_player::outlst) money (t-1) in

  List.rev (init_player_helper [] money num_players)

let init_table num_players money blind = 
  {
    dealer = 0;
    blind = blind;
    participants = init_player num_players money; 
    hole_cards = [];
  }

let init_bet = 
  {
    bet_player = 0;
    bet_amount = 0;
    bet_paid_amt = [];
  }

let init_players_in num_players = 
  let rec helper outlst = function
    | 0 -> outlst 
    | t -> helper (t::outlst) (t-1) in
  List.sort compare (helper [] num_players)

(**  [init_state adv] creates a State.t record with information corresponding
     to the initial state of the [adv] as defined in the adventure file.*)
let init_state game_type num_players money blind =
  {
    game_type = game_type;
    player_number = num_players;
    table = init_table num_players money blind;
    player_turn = 1;
    button = 1;
    players_in = init_players_in num_players;
    (* who bet? *)
    bet = init_bet;
    avail_action = ["fold"; "bet"; "check"]
  }

let game_type st = st.game_type

let player_number st = st.player_number

let table st = st.table

let player_turn st = st.player_turn

let button st = st.button

let players_in st = st.players_in

let bet st = st.bet

let avail_action st = st.avail_action
(* 
let next_player st =
  let curr_player = st.player_turn in

  let rec find ele pos = function
  | [] -> pos
  | h::t -> if ele = h then find  *)

(* check if everyone called the bet *)
let check_bet_amount st =
  let bet_amt = st.bet.bet_amount in
  let rec helper = function
    | [] -> true
    | h::t -> if List.nth st.bet.bet_paid_amt (h-1) = bet_amt then
        helper t
      else false in

  helper st.players_in

(* check if we can go to next round *)
let check_for_next_round st =
  if st.bet.bet_amount = 0 && st.player_turn = button st then true
  else if (st.bet.bet_amount <> 0) && (check_bet_amount st) then true
  else false

type check_result =
  | Legal of t
  | Illegal

let check st =
  if st.player_turn = button st && check_for_next_round st then
    Legal
      {
        game_type = st.game_type;
        player_number = st.player_number;
        table = st.table;
        player_turn = st.player_turn;
        button = st.button;
        players_in = st.players_in;
        (* who bet? *)
        bet = st.bet;
        avail_action = ["fold"; "bet"; "check"]
      }
  else
    Legal
      {
        game_type = st.game_type;
        player_number = st.player_number;
        table = st.table;
        player_turn = st.player_turn;
        button = st.button;
        players_in = st.players_in;
        (* who bet? *)
        bet = st.bet;
        avail_action = ["fold"; "bet"; "check"]
      }
