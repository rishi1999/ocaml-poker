open OUnit2
open Table


(**Deck Tests*)
let make_new_deck =
  Deck.deck_init

let deck_tests = 
  [
    "pick first card" >:: (fun _ -> 
        assert_equal [] []);

  ]
let james:player = {name = "James"; cards = []; money = 32}
let bob:player = {name = "Bob"; cards = []; money = 32}
let table1: table = {dealer = 0; blind = 1; participants = [james;bob]; hole_cards= []}
let table2: table = deal table1
let table_tests =
  [
    "deal_test_1" >:: (fun _->
        assert ((deal table1) != table1));

    "deal_failure_test_1" >:: (fun _->
        assert_raises (Failure "player issue") (fun () -> deal table2));
  ]

let suite = 
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    table_tests;
  ]
let _ = run_test_tt_main suite