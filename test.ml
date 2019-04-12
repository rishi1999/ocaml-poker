open OUnit2
open Table


(**Deck Tests*)
let make_new_deck =
  Deck.deck_init

let deck_tests = 
  [

    "pick first card" >:: (fun _ -> 
        assert_equal (Deck.pick_card) ((Spade:Deck.suit), (Ace:Deck.rank)));

  ]



let suite = 
  "test suite for A6"  >::: List.flatten [
    deck_tests;
  ]
let _ = run_test_tt_main suite