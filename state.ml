open Deck
open Table
open Player
open Hand_evaluator
open Yojson.Basic.Util
open Avatar
open Dynamicinterface

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
  winners: (int * int) list;
}

exception Tie

let prompt str =
  print_newline ();
  print_endline str;
  ANSITerminal.(print_string [blue] "> ")

let rec read_input prompt_str condition type_converter () =
  let retry error_str () =
    print_newline ();
    print_string error_str;
    read_input prompt_str condition type_converter () in
  prompt prompt_str;

  let input = read_line () in
  if input = "quit" then exit 0;
  let var = try (fst type_converter) input with
    | Failure _ ->
      retry (snd type_converter) () in
  if fst condition var then var else retry (snd condition) ()

let read_integer prompt_str ?(condition=((fun x -> true),
                                         "Number does not satisfy conditions.")) () =
  read_input prompt_str condition (int_of_string, "Please enter an integer value.") ()

let read_string prompt_str ?(condition=((fun x -> true),
                                        "String does not satisfy conditions.")) () =
  read_input prompt_str condition ((fun x -> x), "Please enter a valid string.") ()

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
      bet_paid_amt = List.rev( update_bet_paid
                                 [] st.player_turn amount st.bet.bet_paid_amt;)
    } in

  {
    st with
    table;
    player_turn = get_next_player st;
    bet = bet';
    avail_action = ["call"; "raise"; "fold";];
    players_played = st.player_turn :: st.players_played;
  }

let rec find_stack player = function
  | [] -> 0
  | h::t -> if h.id = player then h.money else find_stack player t

(** [pay_blinds st] is the state after the first two players,
    the small blind and the big blind, have paid their blinds.
    Requires: [st] is a valid state where it has just started a new hand. *)
let pay_blinds st =
  let rec pick_blind st amt =
    if st.table.blind <= find_stack st.player_turn st.table.participants then
      money_to_pot st amt
    else
      let remove target lst = List.filter (fun x -> not (x = target)) lst in
      let pl = st.player_turn in
      pick_blind
        {
          st with
          players_in = remove pl st.players_in;
          player_turn = get_next_player st;
          bet =
            {
              st.bet with
              bet_paid_amt = (List.remove_assoc pl st.bet.bet_paid_amt);
            };
        }
        amt in
  let st = pick_blind st (st.table.blind / 2) in
  pick_blind st st.table.blind

(** [init_players game_type num_players money] returns the sorted list of players,
    with length of num_players and everyone's money is equal to the
    input money. *)
let init_players game_type num_players money =
  let rec init_players' acc money = function
    | id when id > num_players -> acc
    | id when game_type > 0 && id = 2 ->
      let curr_player =
        {
          id;
          name = "AI";
          cards = [];
          money;
          wins = 0;
          losses = 0;
          avatar_id = 11;
          consecutive_wins = 0;
          orig_id = 11;
        } in
      init_players' (curr_player :: acc) money (id + 1)
    | id ->
      let name = read_string ("Enter player " ^(string_of_int) id^ "'s name.")
          ~condition:((fun x -> String.length x <= 10
                                && String.length x >= 1)
                     ,"Max length: 10 characters.") () in
      ANSITerminal.(print_string [ANSITerminal.default] array_choice);

      let valid_id = read_integer ("Choose " ^ name ^ "'s avatar.")
          ~condition:((fun x -> x >= 1 && x <= 10),
                      "Please pick a number between 1 and 10.") () - 1 in

      let curr_player =
        {
          id;
          name;
          cards = [];
          money;
          wins = 0;
          losses = 0;
          avatar_id = valid_id;
          consecutive_wins = 0;
          orig_id = valid_id;
        } in
      init_players' (curr_player :: acc) money (id + 1) in
  (init_players' [] money 1)

(** [init_table game_type num_players money blind] returns the list of type
    Player.player, with length of num_players and everyone's money is equal
    to the input money. *)
let init_table game_type num_players money blind =
  Table.deal {
    pot = 0;
    blind;
    participants = init_players game_type num_players money;
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
  if List.length st.table.board = 0 then
    if (st.player_turn = fst
          (
            try List.nth st.bet.bet_paid_amt 1 with
            | Failure _ ->
              print_newline ();
              print_endline
                "Not enough players remaining with sufficient funds.";
              print_newline ();

              let lst = st.table.participants in

              List.iter
                (fun x ->
                   print_endline

                     ((find_participant st x.id).name ^ " ended with $" ^
                      (string_of_int ((find_stack x.id st.table.participants)
                                      + st.table.blind*3/2)) ^
                      "!");
                ) lst;
              print_newline ();


              ANSITerminal.(print_string [yellow] "Game Over!");
              print_newline ();
              print_newline ();
              exit 0
          )
       ) && (st.bet.bet_amount = st.table.blind)
    then
      {
        st with
        avail_action = ["check"; "raise"; "fold"; "show"]
      }
    else
      {
        st with
        avail_action = ["call"; "raise"; "fold"; "show"]
      }
  else
  if st.bet.bet_amount = 0 then
    {
      st with
      avail_action = ["check"; "bet"; "fold"; "show"]
    }
  else
    {
      st with
      avail_action = ["call"; "raise"; "fold"; "show"]
    }


let filter_busted_players st =
  let rec helper outlst = function
    | [] -> outlst
    | h::t ->
      let player_money = (find_participant st h).money in
      if player_money > 0 then helper (h::outlst) t
      else
        helper (outlst) t in
  {st with
   players_in = List.rev (helper [] st.players_in)}

let init_state game_type num_players money blind =
  {
    game_type;
    num_players;
    table = init_table game_type num_players money blind;
    player_turn = 1;
    button = num_players;
    players_in = init_players_in num_players;
    players_played = [];
    bet = init_bet (init_players_in num_players);
    avail_action = ["bet"; "check"; "fold"];
    winners = [];
  } |> filter_busted_players |> pay_blinds |> get_avail_action

let game_type st = st.game_type
let num_players st = st.num_players
let table st = st.table
let player_turn st = st.player_turn
let button st = st.button
let players_in st = st.players_in
let bet st = st.bet
let avail_action st = st.avail_action
let bet_paid_amt st = st.bet.bet_paid_amt
let winners st = st.winners


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
    if List.length st.bet.bet_paid_amt > 1 then if
      st.player_turn = fst (List.nth st.bet.bet_paid_amt 1) then
        not (st.bet.bet_amount = st.table.blind ) && are_all_bets_equal st

      else (are_all_bets_equal st &&
            has_everyone_played st)
    else
      are_all_bets_equal st &&
      has_everyone_played st
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

let winners st =
  let board = st.table.board in
  let all_part = st.table.participants in
  let p_in = st.players_in in
  if List.length p_in = 1 then [(find_participant st (List.hd p_in), 0)]
  else
    let players_in = List.filter (fun player -> List.mem player.id p_in) all_part in
    let rec player_ranker participants outlist =
      match participants with
      | [] -> List.rev outlist
      | p :: t -> let rank = seven_list_eval (p.cards @ board) in
        player_ranker t ((p,rank) :: outlist) in
    let ranked_players = player_ranker players_in [] in
    let min_rank = List.fold_left (fun accu (id,rank) ->
        min accu rank) 7463 ranked_players in
    List.filter (fun (id,rank) -> rank = min_rank) ranked_players

(** [go_next_round] st ends the current round or the current hand and
    returns the state with the next round. *)
let go_next_round st =
  if is_hand_complete st then
    let winner_pls = List.map (fun (p,_) -> p) (winners st) in
    let hand_qualities = List.map (fun (_,r) -> r) (winners st) in

    let num_winners = List.length winner_pls in
    let profit = st.table.pot / num_winners in

    (* note: any money that doesn't divide well is given to the house *)
    let winner_pls = List.map (fun pl ->
        {
          pl with
          money = pl.money + profit;
          wins = pl.wins + 1;
          consecutive_wins = pl.consecutive_wins + 1;
          (*avatar_id = if pl.consecutive_wins < 5
            then 9 + pl.consecutive_wins + 1
            else pl.avatar_id*)
        }
      ) winner_pls in

    let celebration_str = if num_winners = 1 then
        (List.hd winner_pls).name ^ " wins $" ^ string_of_int profit
        ^ " with " ^ Hand_evaluator.rank_mapper (List.hd hand_qualities) ^ "!"
        ^ "\n" ^ dyndescrp (List.hd winner_pls)
      else
        let names = List.map (fun pl -> pl.name) winner_pls in
        "Winners:\n" ^ (List.fold_left (fun a b -> a ^ "    " ^ b) "" names)
        ^ " and each earned $" ^ string_of_int profit ^ "!"
    in
    print_newline ();
    print_newline ();
    ANSITerminal.(print_string [yellow] celebration_str);
    ignore (read_line ());

    let winner_pl_ids = List.map (fun pl -> pl.id) winner_pls in

    let loser_pls = List.filter (fun pl -> not (List.mem pl.id winner_pl_ids))
        st.table.participants |> List.map (fun pl ->
        {
          pl with
          losses = pl.losses + 1;
          consecutive_wins = 0;
          avatar_id = pl.orig_id;
        }
      ) in

    let participants = winner_pls @ loser_pls in

    let table = {
      st.table with
      participants;
    } |> Table.clear_round in

    let button = if st.button + 1 > st.num_players then 1
      else st.button + 1 in

    let players_in = hand_order st.num_players button in

    {
      st with
      table = Table.deal table;
      bet = init_bet players_in;
      player_turn = List.nth players_in 0;
      button;
      players_in;
      players_played = [];
      winners = List.map2 (fun id r -> (id,r)) winner_pl_ids hand_qualities;
    }

    |> filter_busted_players |> pay_blinds |> get_avail_action

  else
    {
      st with
      table = Table.add_to_board st.table;
      bet = init_bet st.players_in;
      player_turn = List.nth st.players_in 0;
      players_played = [];
    }

let continue_game st = {
  st with
  winners = [];
}

let winning_players st = st.winners

let calculate_pay_amt st =
  let cur_bet_size = st.bet.bet_amount in
  let rec get_bet_amt target = function
    | [] -> 0
    | (p, a)::t -> if p = target then a else get_bet_amt target t in

  Pervasives.abs(cur_bet_size - get_bet_amt st.player_turn st.bet.bet_paid_amt)

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
  else Illegal "You can't check right now!"

let call st =
  if List.mem "call" st.avail_action then
    if calculate_pay_amt st <=
       (find_stack st.player_turn st.table.participants) then
      let t = money_to_pot st (calculate_pay_amt st) in
      if is_round_complete t || is_hand_complete t then
        Legal (get_avail_action (go_next_round t))
      else
        Legal (get_avail_action t)
    else Illegal "You don't have enough money to do that!"
  else Illegal "You can't call right now!"

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
  else Illegal "You can't fold right now!"

let bet_or_raise amt st comm_str =
  if List.mem comm_str st.avail_action then
    if amt < st.table.blind then
      Illegal "You have to bet at least the blind!"
    else if comm_str = "raise" && (amt < 2*st.bet.bet_amount &&
                                   amt <> (find_participant st 
                                             st.player_turn).money) then
      Illegal "You have to raise at least twice the bet!"
    else if amt > (find_stack st.player_turn st.table.participants) then
      Illegal "You don't have enough money to do that!"
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

let save file_name st =
  let rec get_participants outlst = function
    | [] -> outlst
    | h::t -> let x = `Assoc
                  [
                    ("id", `Int h.id);
                    ("name", `String h.name);
                    ("card1", `Int (Deck.int_converter (List.hd h.cards)));
                    ("card2", `Int (Deck.int_converter (List.hd 
                                                          (List.tl h.cards))));
                    ("money", `Int h.money);
                    ("avatar_id", `Int h.avatar_id);
                    ("wins", `Int h.wins);
                    ("losses", `Int h.losses);
                    ("consecutive_wins", `Int h.consecutive_wins);
                    ("orig_id", `Int h.orig_id);
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

  let participants_json = get_participants [] st.table.participants in
  let bet_amt = get_bet_amt [] st.bet.bet_paid_amt in

  let winners_to_json =
    let rec helper playerlst ranklst = function
      | [] -> (playerlst, ranklst)
      | (player, rank)::t ->
        helper (`Int player::playerlst) (`Int rank::ranklst) t in
    helper [] [] (st.winners) in

  let win_players = List.rev (fst winners_to_json) in
  let ranks = List.rev (snd winners_to_json) in

  Yojson.to_file (file_name ^ ".json") (
    `Assoc
      [
        ("game_type", `Int st.game_type);
        ("num_players", `Int st.num_players);
        ("table",
         `Assoc
           [("pot", `Int st.table.pot);
            ("blind", `Int st.table.blind);
            ("participants", `List (List.rev participants_json));
            ("board", `List (List.rev (get_cards_int [] st.table.board)));
           ]);
        ("player_turn", `Int st.player_turn);
        ("button", `Int st.button);
        ("players_in", `List (List.map (fun x -> `Int x) st.players_in));
        ("players_played",
         `List (List.map (fun x -> `Int x) st.players_played));
        ("bet",
         `Assoc
           [
             ("bet_player", `Int st.bet.bet_player);
             ("bet_amount", `Int st.bet.bet_amount);
             ("bet_paid_amt", `List (List.rev bet_amt));
           ];
        );
        ("avail_action",
         `List (List.map (fun x -> `String x) st.avail_action));
        ("winners", `Assoc [("players", `List win_players);
                            ("ranks", `List ranks)]);
        ("deck", `List (List.map (fun x -> `Int x)
                          (List.map (Deck.int_converter) !Deck.current_deck))
        );
      ]
  );
  st

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
    name = json |> member "name" |> to_string;
    money = json |> member "money" |> to_int;
    wins = json |> member "wins" |> to_int;
    losses = json |> member "losses" |> to_int;
    avatar_id = json |> member "avatar_id" |> to_int;
    consecutive_wins = json |> member "consecutive_wins" |> to_int;
    orig_id = json |> member "orig_id" |> to_int;
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

  let winners_of_json json =
    let players = json |> member "players" |> to_list |> List.map to_int in
    let ranks =  json |> member "ranks" |> to_list |> List.map to_int in
    List.map2 (fun pl r -> (pl,r)) players ranks
  in

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
    avail_action = json |> member "avail_action" |> to_list
                   |> List.map (fun x -> to_string x);
    winners = json |> member "winners" |> winners_of_json;
  } in

  let parse json =
    current_deck := json |> member "deck" |> to_list |> List.map to_int |>
                    List.map (card_inverter);
    deck_size := List.length (!current_deck);
    try t_of_json json
    with Type_error (s, _) -> failwith ("Parsing error: " ^ s) in

  parse json

let command_to_function = Command.(function
    | Check -> check
    | Bet amt -> bet' amt
    | Call -> call
    | Raise amt -> raise' amt
    | Fold -> fold
    | _ -> failwith "ERROR: unsupported command"
  )