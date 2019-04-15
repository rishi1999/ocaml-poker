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
  num_players: int;
  table: Table.table;
  player_turn: int;
  button : int;
  (* players playing *)
  players_in: int list;
  (* who bet? *)
  bet: bet;
  avail_action: string list;
  is_new_round : bool;
}

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
    game_type;
    num_players;
    table = init_table num_players money blind;
    player_turn = 1;
    button = 1;
    players_in = init_players_in num_players;
    (* who bet? *)
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
(** [hand_order num_players button] returns an integer lists
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

(* check if everyone called the bet *)
let are_all_bets_equal st = List.for_all
    (fun paid -> paid = st.bet.bet_amount) st.bet.bet_paid_amt

(* check if we can go to next round *)
let is_round_complete st = st.is_new_round && are_all_bets_equal st

type check_result =
  | Legal of t
  | Illegal

(* TODO not done *)
let check st =
  if is_round_complete st then
    Legal (go_next_round st)
  else
    Legal
      {
        st with
        player_turn = st.player_turn + 1
      }
