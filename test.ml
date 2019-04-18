open OUnit2
open Table
open Player
open Hand_evaluator
open Deck
open State



let make_new_deck =
  Deck.deck_init

let deck_tests =
  [
    "pick first card" >:: (fun _ ->
        assert_equal [] []);
    "convert 9C" >:: (fun _ ->
        assert_equal 28 (int_converter (Clubs, Nine)));
    "pick_card_test" >:: (fun _ ->
        assert_equal 4 (List.length (pick_cards 4)));
    "int_converter Test 1" >:: (fun _ ->
        assert_equal 4 (List.length (pick_cards 4)));
  ]

let james = {id = 0; cards = []; money = 32}
let bob = {id = 1; cards = []; money = 23}
let julian = {id = 2; cards = [(Diamonds, Five)]; money = 32}

let player_tests =
  [
    "ID Test1" >:: (fun _->
        assert_equal 0  (id james));

    "ID Test2" >:: (fun _->
        assert_equal 1  (id bob));

    "Card Test1" >:: (fun _->
        assert_equal  [(Diamonds, Five)] (cards julian));

    "Money Test1" >:: (fun _->
        assert_equal  32 (money julian));

  ]

let james = {id = 0; cards = []; money = 32}
let bob = {id = 1; cards = []; money = 32}
let table1 = {pot = 0; blind = 1; participants = [james;bob]; board= []}
let table2 = deal table1


let table2_players table2 = table2.participants
let james_cards_2 table2_players = match table2_players with
  | {
    id = s;
    cards = c;
    money = m
  } :: t -> c
  | _ -> failwith "table2 not dealt"

let table1_players = table1.participants
let james_cards = match table1_players with
  | {
    id = s;
    cards = c;
    money = m
  } :: t -> c
  | _ -> failwith "table2 not dealt"

let empty : Deck.card list = []
let jimmy = {id = 1; cards = [(Spades, Ace);(Clubs, Ace)]; money = 32}
let bobby = {id = 2; cards = [(Spades, Two);(Clubs, Two)]; money = 32}
let alice = {id = 3; cards = [(Spades, Three); (Hearts, Four)]; money = 42}
let state_table_1 = {pot = 0; blind = 2; participants = [jimmy; bobby; alice];
                     board = [(Hearts, Ace);(Diamonds, Ace);(Spades, King);
                              (Hearts, King); (Hearts, Three)]}
let state_bet_1 =
  {
    bet_player = 1;
    bet_amount = 0;
    bet_paid_amt = [(0,0)];
  }

let state1 =
  {
    game_type = 0;
    num_players = 2;
    table = state_table_1;
    player_turn = 0;
    button = 0;
    players_in = [1;2;3];
    players_played = [];
    bet = state_bet_1;
    avail_action = ["fold"];
    winner = (-1,0);
  }
let state2 = {state1 with players_in = [2]}
let state3 = {state1 with players_in = [3]}
let state4 = {state1 with players_in = [2; 3]}

let state_tests =
  [
    "hand_order_test1" >:: (fun _ ->
        assert_equal [4; 5; 1; 2; 3] (hand_order 5 3 ));
    "winner_test_1" >:: (fun _ ->
        assert_equal jimmy (fst (winner state1)));
    "winner_test_2" >:: (fun _ ->
        assert_equal bobby (fst (winner state2)));
    "winner_test_3" >:: (fun _ ->
        assert_equal alice (fst (winner state3)));
    "winner_test_4" >:: (fun _ ->
        assert_equal alice (fst (winner state3)));
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
        assert (table1 <> (add_to_board (table1))));
  ]


let a = 7*4+0
let b = 2*4+0
let c = 2*4+3
let d = 7*4+1
let e = 2*4+2
let f = 10*4+0
let g = 4*4+0
let h = 0*4+0
let i = 7*4+2

let list1 = [(Spades, Two);(Clubs, Two)]

let cards_1 = list1 @ [(Hearts, Ace);(Diamonds, Ace);
                       (Spades, King);
                       (Hearts, King); (Hearts, Three)];;


let list2 = [(Hearts, Ace); (Diamonds, Ace); (Spades, King); (Hearts, King);
             (Hearts, Three)]
let cards_2 = list1 @ list2
let cards_3 = [(Spades, Three); (Hearts, Four)] @ [(Hearts, Ace);
                                                   (Diamonds, Ace);
                                                   (Spades, King);
                                                   (Hearts, King);
                                                   (Hearts, Three)];;
let single_card = [(Spades, Ace)]



let hand_evaluator_tests =
  [
    "4_full_house" >:: (fun _->
        assert_equal 292 (seven_eval a b c d e f g ));

    "9_full_house" >:: (fun _->
        assert_equal 236 (seven_eval a b c d e h i ));

    "rank_mapper_test" >:: (fun _->
        assert_equal "Royal Flush" (rank_mapper 1));

    "seven_list_eval_test1" >:: (fun _ ->
        assert_equal 2477 (seven_list_eval cards_1));

    "seven_list_eval_test2" >:: (fun _ ->
        assert_equal 2477 (seven_list_eval cards_2));

    "seven_list_eval_test3" >:: (fun _ ->
        assert_equal 2476 (seven_list_eval cards_3));

  ]

let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    table_tests;
    hand_evaluator_tests;
    state_tests;
    player_tests;
  ]
let _ = run_test_tt_main suite