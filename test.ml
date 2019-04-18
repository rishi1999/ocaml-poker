open OUnit2
open Table
open Player
open Hand_evaluator
open Deck
open State

(**Deck Tests*)
let make_new_deck =
  Deck.deck_init

let deck_tests =
  [
    "pick first card" >:: (fun _ ->
        assert_equal [] []);
    "convert 9C" >:: (fun _ ->
        assert_equal 28 (int_converter (Clubs, Nine)));
  ]
let james:player = {id = 0; cards = []; money = 32}
let bob:player = {id = 1; cards = []; money = 32}
let table1: table = {dealer = 0; blind = 1; participants = [james;bob]; board= []}
let table2: table = deal table1


let table2_players table2 =  match table2 with
    { dealer = d ; blind = b; participants = p ; _ } -> p
let james_cards_2 table2_players = match table2_players with
  | {id = s; cards = c; money = m}::t -> c
  | _ -> failwith "table2 not dealt"

let table1_players=  match table1 with
    { dealer = d ; blind = b; participants = p ; _ } -> p
let james_cards= match table1_players with
  | {id = s; cards = c; money = m}::t -> c
  | _ -> failwith "table2 not dealt"

let empty: (Deck.suit * Deck.rank) list = []

let jimmy = {id = 1; cards = [(Spades, Ace);(Clubs, Ace)]; money = 32}
let bobby = {id = 2; cards = [(Spades, Two);(Clubs, Two)]; money = 32}
let alice = {id = 3; cards = [(Spades, Three); (Hearts, Four)]; money = 42}

let tie1 = {id = 1; cards = [(Clubs, Two);(Clubs, Five)]; money = 50}
let tie2 = {id = 2; cards = [(Clubs, Four);(Hearts, Five)]; money = 50}

let state_table_1 = {dealer = 1; blind = 2; participants = [jimmy;bobby;alice];
                     board = [(Hearts, Ace);(Diamonds, Ace);(Spades, King);
                              (Hearts, King); (Hearts, Three)]}
let tie_table = {dealer = 1; blind = 5; participants = [tie1;tie2];
                 board = [(Hearts, Ace);(Diamonds, Ace);(Spades, King);
                          (Hearts, King); (Hearts, Three)]}
let state_bet_1 = {
  bet_player = 1;
  bet_amount = 0;
  bet_paid_amt = [(0,0)];
}

let state1 = {
  game_type = 0;
  num_players = 2;
  table = state_table_1;
  player_turn = 0;
  button = 0;
  players_in = [1;2;3];
  players_played = [];
  bet = state_bet_1;
  avail_action = ["fold"];
  winners = [];
}
let state2 = {state1 with players_in = [2]}
let state3 = {state1 with players_in = [3]}
let state4 = {state1 with players_in = [2;3]}
let tied_state = {
  game_type = 0;
  num_players = 2;
  table = tie_table;
  player_turn = 1;
  button = 2;
  players_in = [1;2];
  players_played = [];
  bet = state_bet_1;
  avail_action = ["fold"];
  winners = [];
}

let my_res st =
  let res =
    winners st in
  print_newline ();
  print_endline "The Begin";
  print_string "length: ";
  print_endline (string_of_int ((List.length res)));
  print_string "head: ";
  print_endline (string_of_int ((List.hd res).id));
  print_string "last elem: ";
  print_endline (string_of_int ((List.hd (List.rev res)).id));
  print_endline "The The End";
  print_newline ();
  res

(** State Tests*)
let state_tests =

  [
    "hand_order_test1" >:: (fun _ ->
        assert_equal [4; 5; 1; 2; 3] (hand_order 5 3 ));

    "winner_test_1" >:: (fun _ ->
        assert_equal [jimmy] (my_res state1));
    "winner_test_2" >:: (fun _ ->
        assert_equal [bobby] (my_res state2));
    "winner_test_3" >:: (fun _ ->
        assert_equal [alice] (my_res state3));
    "winner_test_4" >:: (fun _ ->
        assert_equal [alice] (my_res state3));
    "winner_test_tie" >:: (fun _ ->
        assert_equal [tie1; tie2] (
          my_res tied_state
        ));
    (*  "get_players_in_test" >:: (fun _->
          assert_equal [bobby;alice] (get_players_in state4)); *)

  ]

let table_tests =
  [
    "deal_test_1" >:: (fun _->
        assert ((deal table1) <> table1));
    "deal_test_2" >:: (fun _->
        assert_equal james_cards empty);
    "deal_failure_test_1" >:: (fun _->
        assert_raises (Failure "player has non 0 cards") (fun () -> deal table2));
    "add_to_hole_test_1" >:: (fun _->
        assert (table1 <> (add_to_hole (table1))));
  ]

let a = 7*4+0
let b = 2*4+0
let c = 2*4+3
let d = 7*4+1
let e = 2*4+2
let f = 10*4+0
let g = 4*4+0

let h = 0 * 4 + 0
let i = 7 * 4 +2

let hand_evaluator_tests =
  [
    "4_full_house" >:: (fun _->
        assert_equal 292 (seven_eval a b c d e f g ));

    "9_full_house" >:: (fun _->
        assert_equal 236 (seven_eval a b c d e h i ));

  ]

let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    table_tests;
    hand_evaluator_tests;
    state_tests;
  ]
let _ = run_test_tt_main suite