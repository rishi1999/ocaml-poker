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

let init_players num_players money =
  let rec init_players' acc money = function
    | 0 -> acc
    | id -> let curr_player =
              {
                id;
                action = None;
                cards = [];
                money;
              } in
      init_players' (curr_player :: acc) money (id - 1) in
  init_players' [] money num_players

let init_table num_players money blind =
  {
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

(* WHAT IS THE POINT OF THIS FUNCTION? -- removed the sort since the elements already seem to be ordered *)
let init_players_in num_players =
  let rec init_players_in' acc = function
    | 0 -> acc
    | t -> init_players_in' (t :: acc) (t - 1) in
  (*List.sort compare*) (init_players_in' [] num_players)

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
  (st.bet.bet_amount = 0 && st.player_turn = st.button) ||
  (st.bet.bet_amount <> 0) && (check_bet_amount st)

type check_result =
  | Legal of t
  | Illegal

let check st =
  if st.player_turn = st.button && check_for_next_round st then
    Legal
      {
        st with
        avail_action = ["fold"; "bet"; "check"];
      }
  else
    Legal
      {
        st with
        avail_action = ["fold"; "bet"; "check"];
      }
