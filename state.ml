open Deck
open Table
open Player
open Hand_evaluator
open Yojson.Basic.Util

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
  winner : (int*int);
}
exception Tie

let prompt str =
  print_newline ();
  print_endline str;
  ANSITerminal.(print_string [blue] "> ")

(** [get_next_player] st returns the id of the player that has
    to act next.
    Requires: st.players_in is not an empty list *)
let get_next_player st =
  let rec helper = function
    | x -> let guess = if x + 1 > st.num_players then 1 else x + 1 in
      if List.mem guess st.players_in then guess else helper (guess) in
  helper st.player_turn

let find_participant st target =
  let rec find_participant' target = function
    | [] -> failwith "ERROR: player does not exist"
    | h :: t -> if (Player.id h) = target then h
      else find_participant' target t in
  find_participant' target (Table.participants st.table)

(** [money_to_pot] st amount returns the state after the player has put
    amount of money into the pot, either through betting or raising.
    Requires: st is a valid state of the game
              the player has at least amount in his stack *)
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
      pot = st.table.pot + amount;
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

(** [pay_blinds] st returns the state after the first two players,
    the small blind and the big blind, have paid their blinds.
    Requires: st is a valid state where it has just started a new hand *)
let pay_blinds st =
  let small_blind = money_to_pot st (st.table.blind / 2) in
  money_to_pot small_blind st.table.blind

(** [init_players] num_players money returns the sorted list of players,
    with length of num_players and everyone's money is equal to the
    input money. *)
let init_players num_players money =
  let rec init_players' acc money = function
    | id when id > num_players -> acc
    | id ->
      prompt ("Enter player " ^ (string_of_int) id ^ "'s name.");
      let name = read_line () in
      let curr_player =
        {
          id;
          name;
          cards = [];
          money;
        } in
      init_players' (curr_player :: acc) money (id + 1) in
  (init_players' [] money 1)

(** [init_table] num_players money blind returns the list of type
    Player.player, with length of num_players and everyone's money is equal
    to the input money. *)
let init_table num_players money blind =
  Table.deal {
    pot = 0;
    blind;
    participants = init_players num_players money;
    board = [];
  }

(** [init_bet_paid_amt] players_in returns a list of elements in the form
    (player_id, 0) for the second value of the tuple to denote the total amount
    of that the player has put in the pot in this round. *)
let init_bet_paid_amt players_in =
  let rec helper lst = function
    | [] -> lst
    | h::t -> helper ((h,0)::lst) t in
  List.rev (helper [] players_in)

(** [init_players_in] num_players returns a list containing all players' id *)
let init_players_in num_players =
  let rec init_players_in' acc = function
    | 0 -> acc
    | t -> init_players_in' (t :: acc) (t - 1) in
  init_players_in' [] num_players

(** [init_bet] players_in initializes a type bet*)
let init_bet players_in =
  {
    bet_player = 0;
    bet_amount = 0;
    bet_paid_amt = init_bet_paid_amt players_in;
  }

let hand_order num_players button =
  let rec list_builder start term outlist =
    if start > term then outlist
    else list_builder start (term - 1) (term :: outlist) in
  let second = list_builder 1 button [] in
  let first = list_builder (button + 1) num_players [] in
  first @ second

let get_avail_action st =
  if st.bet.bet_amount = 0 then
    {
      st with
      avail_action = ["check"; "bet"; "fold"]
    }
  else if List.length st.table.board = 0 &&
          st.bet.bet_amount = st.table.blind &&
          st.player_turn = match st.bet.bet_paid_amt with
          | [] -> failwith "ERROR: no bets exist"
          | h :: [] -> failwith "ERROR: only one bet exists"
          | h :: (p,a) :: t -> p
  then
    {
      st with
      avail_action = ["check"; "bet"; "fold"]
    }
  else
    {
      st with
      avail_action = ["call"; "raise"; "fold"]
    }


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
    winner = (-1,0);
  } |> pay_blinds |> get_avail_action

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

(** [is_round_complete st] is true if the game is
    ready to move on to the next round. *)
let is_round_complete st =
  if List.length st.table.board = 0 then
    (are_all_bets_equal st &&
     has_everyone_played st) &&
    not (st.player_turn = fst (List.nth st.bet.bet_paid_amt 1) ||
         st.bet.bet_amount = st.table.blind)
  else
    are_all_bets_equal st &&
    has_everyone_played st


(** [is_hand_complete st] is true if hand is complete. *)
let is_hand_complete st =
  let everyone_folded = (List.length st.players_in < 2) in
  let after_river = (List.length st.table.board = 5) in

  everyone_folded || after_river && is_round_complete st

let rec get_players_in part players_in ls = match players_in with
  | a :: t when List.mem a.id part -> get_players_in part t (a :: ls)
  | a :: t -> get_players_in part t ls
  | [] -> List.rev ls

let winner st =
  let board = st.table.board in
  let all_part = st.table.participants in
  let p_in = st.players_in in

  (** ranks returns a list of ranks of the hands of the list players*)
  let rec ranks participants (board : Deck.card list) lst =
    match participants with
    | [] -> List.rev lst
    | p :: t -> ranks t board ((seven_list_eval (p.cards @ board)) :: lst)
  in

  (** best_rank gets the best rank in the list of hands*)
  let rec best_player ls acc = match ls with
    | [] -> acc
    | a :: t when a < acc -> best_player t a
    | a :: t when a > acc -> best_player t acc
    | _ -> raise Tie
  in

  (** [get_player_in target ls acc] is the integer position
      of the list of the best player. *)
  let rec get_player_int target ls acc = match ls with
    | a :: b when a = target -> acc
    | a :: b -> get_player_int target b (acc + 1)
    | [] -> failwith "ERROR: no best player" in

  let part = get_players_in p_in all_part [] in
  let rlist = ranks part board [] in
  let best_rank = (best_player rlist 7463) in
  let num_winner = get_player_int best_rank rlist 0 in

  (List.nth part num_winner, best_rank)

(** [go_next_round] st ends the current round or the current hand and
    returns the state with the next round. *)
let go_next_round st =
  if is_hand_complete st then
    let winner_player = if List.length st.players_in = 1 then
        List.hd st.players_in else
        try (fst (winner st)).id with Tie -> -2
    in
    let win_amount = st.table.pot in
    let player_won = find_participant st winner_player in
    let player_paid =
      {player_won with money = player_won.money + win_amount} in
    let rec update_parcipant target player outlst = function
      | [] -> outlst
      | h::t -> if h.id = target then
          update_parcipant target player (player::outlst) t
        else update_parcipant target player (h::outlst) t in
    let updated_participants = update_parcipant winner_player player_paid []
        st.table.participants in
    let updated_table = {
      st.table with
      participants = updated_participants;
    } in
    let cleared = Table.clear_round updated_table in
    let button_updated = if st.button + 1 > st.num_players then 1
      else st.button + 1 in
    let players_in_updated = hand_order st.num_players button_updated in
    if List.length st.players_in = 1 then
      let interim_state = {
        st with
        table = Table.deal (cleared);
        bet = init_bet players_in_updated;
        player_turn = List.nth players_in_updated 0;
        button = button_updated;
        players_in = players_in_updated;
        players_played = [];
        winner = (winner_player, 0);
      } in
      interim_state |> pay_blinds |> get_avail_action
    else
      let interim_state = {
        st with
        table = Table.deal (cleared);
        bet = init_bet players_in_updated;
        player_turn = List.nth players_in_updated 0;
        button = button_updated;
        players_in = players_in_updated;
        players_played = [];
        winner = (winner_player, (snd (winner st)));
      } in
      interim_state |> pay_blinds |> get_avail_action
  else
    let card_added = Table.add_to_board st.table in
    {
      st with
      table = card_added;
      bet = init_bet st.players_in;
      player_turn = List.nth st.players_in 0;
      players_played = [];
    }

let continue_game st = {st with winner = (-1,0)}

let winning_player st = st.winner

(** [calculate_pay_amt] st returns the amount that the current player has
    to put into the pot to call either a bet or a raise *)
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
      Legal (get_avail_action (go_next_round checked))
    else
      Legal
        (get_avail_action checked)
  else Illegal "You can't do that right now!"

let call st =
  if List.mem "call" st.avail_action then
    if calculate_pay_amt st <=
       (find_stack st.player_turn st.table.participants) then
      let t = money_to_pot st (calculate_pay_amt st) in
      if is_round_complete t || is_hand_complete t then
        Legal (get_avail_action (go_next_round t))
      else
        Legal (get_avail_action t)
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
      Legal (get_avail_action (go_next_round t))
    else
      Legal (get_avail_action t)
  else Illegal "You can't do that right now!"

let stack st =
  let players = List.sort compare st.players_in in
  let print_stack player =
    let color = ANSITerminal.(if player = player_turn st then
                                green else default) in
    ANSITerminal.print_string [color]
      ((Player.name (find_participant st player)) ^
       " has $" ^
       (string_of_int
          (find_stack player st.table.participants)) ^
       ".");
    print_newline () in

  List.iter print_stack (List.sort compare players);
  Legal st

let bet_or_raise amt st comm_str =
  if List.mem comm_str st.avail_action then
    if amt < st.table.blind then
      Illegal "You have to bet at least the blind!"
    else if comm_str = "raise" && amt < 2*st.bet.bet_amount then
      Illegal "You have to raise at least twice the bet!"
    else if amt > (find_stack st.player_turn st.table.participants) then
      Illegal "You're not very liquid at the moment...."
    else if comm_str = "bet" then
      Legal (get_avail_action (money_to_pot st amt))
    else
      let rec get_paid_amt = function
        | [] -> 0
        | (player,amt)::t -> if st.player_turn = player then amt
          else get_paid_amt t in
      let curr_paid_amt = get_paid_amt st.bet.bet_paid_amt in
      let temp_state =  (money_to_pot st amt) in
      let updated_bet =
        {
          temp_state.bet with
          bet_amount = curr_paid_amt + amt;
        } in
      Legal (get_avail_action
               {
                 temp_state with
                 bet = updated_bet;
               })
  else Illegal "You can't do that right now!"

let bet' amt st = bet_or_raise amt st "bet"
let raise' amt st = bet_or_raise amt st "raise"

(* SAVE / LOAD NEEDS IMPLEMENTATION *)


(* game_type: int;
   num_players: int;
   table: Table.table;
   player_turn: int;
   button: int;
   players_in: int list;
   players_played: int list;
   bet: bet;
   avail_action: string list;
   winner : int; *)

(* type table = {
   pot: int;
   blind: int;
   participants: Player.player list;
   board: (Deck.suit * Deck.rank) list;
   } *)

(* type player = 
   {
    id: int;
    cards: (Deck.suit * Deck.rank) list;
    money: int;
   } *)

(* type bet = {
   bet_player: int;
   bet_amount: int;
   bet_paid_amt: (int*int) list;
   } *)

let save st = 
  let rec get_participants outlst = function
  | [] -> outlst
  | h::t -> let x = `Assoc [("id", `Int h.id); 
    ("card1", `Int (Deck.int_converter (List.hd h.cards)));
    ("card2", `Int (Deck.int_converter (List.hd (List.tl h.cards))));
    ("money", `Int h.money);
    ] in
    get_participants (x::outlst) t in

  let rec get_cards_int outlst = function
    | [] -> outlst
    | h::t -> get_cards_int (`Int (Deck.int_converter h)::outlst) t in

  let rec get_bet_amt outlst = function
  | [] -> outlst
  | (player,paid)::t -> let x = `Assoc [("id", `Int player); 
    ("paid", `Int paid);
    ] in
    get_bet_amt (x::outlst) t in
(* 
  let rec get_winner outlst = function
  | [] -> outlst
  | (player, rank) :: t -> 
    let x = `Assoc [("id", `Int player); 
    ("rank", `Int rank);] in
    get_winner (x::outlst) t in *)

  let participants_json = get_participants [] st.table.participants in
  let bet_amt = get_bet_amt [] st.bet.bet_paid_amt in
  (* let winners = get_winner [] st.winner in *)

  Yojson.to_file "saved_game.json" (
    `Assoc
    [
    ("game_type", `Int st.game_type);
    ("num_players", `Int st.num_players);
    ("table",
      `List
        [`Assoc
          [("pot", `Int st.table.pot);
            ("blind", `Int st.table.blind);
            ("participants", 
            `List 
                participants_json);
          ]
        ]);
            ("board", 
            `List 
                (get_cards_int [] st.table.board);
              );
    ("player_turn", `Int st.player_turn);
    ("button", `Int st.button);
    ("players_in", `List (List.map (fun x -> `Int x) st.players_in));
    ("players_played", `List (List.map (fun x -> `Int x) st.players_played));
    ("bet", `List
      [ 
        `Assoc [
          ("bet_player", `Int st.bet.bet_player);
          ("bet_amount", `Int st.bet.bet_amount);
          ("bet_paid_amt", `List bet_amt
          );
        ];
      ]);
    ("avail_action", `List (List.map (fun x -> `String x) st.avail_action));
    ("winner", `Assoc [("player", `Int (fst st.winner)); 
                       ("rank", `Int (snd st.winner))]);
    ]
  );
  Legal st

let load json = 

  let card_inverter card_int = 
    let offset = match card_int mod 4 with
      | 0 -> Clubs
      | 1 -> Diamonds
      | 2 -> Hearts
      | 3 -> Spades
      | _ -> failwith "Wrong Card" 
      in
    let rank = match card_int / 4 with
      | 0 -> Two
      | 1 -> Three
      | 2 -> Four
      | 3 -> Five
      | 4 -> Six
      | 5 -> Seven
      | 6 -> Eight
      | 7 -> Nine
      | 8 -> Ten
      | 9 -> Jack
      | 10 -> Queen
      | 11 -> King
      | 12 -> Ace 
      | _ -> failwith "Wrong Card" in

    (offset, rank) in


   let participants_of_json json = {
    id = json |> member "id" |> to_int;
    cards = [(json |> member "card1" |> to_int |> card_inverter);
    (json |> member "card2" |> to_int |> card_inverter);];
    name = "Default";
    money = json |> member "money" |> to_int;
   } in

   let bet_paid_of_json json =
    let id = json |> member "id" |> to_int in
    let money = json |> member "paid" |> to_int in
    (id, money)
   in

   let bet_of_json json = {
    bet_player = json |> member "bet_player" |> to_int;
    bet_amount = json |> member "bet_amount" |> to_int;
    bet_paid_amt = json |> member "bet_paid_amt" |> to_list
    |> List.map bet_paid_of_json;
   }
   in

   let table_of_json json = {
    pot = json |> member "pot" |> to_int;
    blind = json |> member "blind" |> to_int;
    participants = json |> member "participants"
    |> to_list |> List.map participants_of_json;
    board = json |> member "board" |> to_list |> List.map to_int 
    |> List.map card_inverter;
   } in

   let t_of_json json = {
    game_type = json |> member "game_type" |> to_int;
    num_players = json |> member "num_players" |> to_int;
    table = json |> member "table" |> table_of_json;
    player_turn = json |> member "player_turn" |> to_int;
    button = json |> member "button" |> to_int;
    players_in = json |> member "players_in" |> to_list 
    |> List.map (fun x -> to_int x);
    players_played = json |> member "players_played" |> to_list
    |> List.map (fun x -> to_int x);
    bet = json |> member "bet" |> bet_of_json;
    avail_action = [];
    winner = (json |> member "winner" |> to_int, 0);
   } in

   let parse json =
    try t_of_json json
    with Type_error (s, _) -> failwith ("Parscing error: " ^ s) in

   parse json

let command_to_function = Command.(function
    | Check -> check
    | Bet amt -> bet' amt
    | Call -> call
    | Raise amt -> raise' amt
    | Fold -> fold
    | Save -> save
    | _ -> failwith "ERROR: unsupported command"
  )