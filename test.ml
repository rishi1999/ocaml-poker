open OUnit2
open Table
open Player
open Hand_evaluator
open Deck
open State
open Command
open Hand_analysis

let make_new_deck =
  Deck.deck_init



let empty_table = {
  pot=0;
  blind=5;
  participants = [{id = 1; name = "Wilson"; cards = []; money = 500; wins = 0;
                   losses = 0; avatar_id = 0; orig_id = 0;
                   consecutive_wins = 0};
                  {id = 2; name = "Lucy"; cards = []; money = 500; wins = 0;
                   losses = 0; avatar_id = 0; orig_id = 0; 
                   consecutive_wins = 0}];
  board = [];
}


(**Deck Tests. Most exposed deck functions returned unit. For 
   pick_efficient, it always executes by first selecting a random seed
   using Random.self_init() so its output varies and cannot be
   directly tested. *)
let deck_tests = 
  [
    "pick first card" >:: (fun _ -> deck_init ();
                            assert_equal (pick_cards 1) 
                              [(Clubs, Two)]);
    "pick two cards" >:: (fun _ -> deck_init();
                           assert_equal (pick_cards 2) [(Clubs, Two); 
                                                        (Clubs, Three)]);
    "pick 0 cards" >:: (fun _ -> deck_init();
                         assert_equal (pick_cards 0) []);
    "convert 9C" >:: (fun _ -> 
        assert_equal 28 (int_converter (Clubs, Nine)));
    "convert 9C" >:: (fun _ -> 
        assert_equal 13 (int_converter (Diamonds, Five)));
    "convert 9C" >:: (fun _ -> 
        assert_equal 10 (int_converter (Hearts, Four)));
    "convert 9C" >:: (fun _ -> 
        assert_equal 51 (int_converter (Spades, Ace)));
    "convert 9C" >:: (fun _ -> 
        assert_equal 7 (int_converter (Spades, Three)));
    "pick_card_test" >:: (fun _ ->
        assert_equal 4 (List.length (pick_cards 4)));
    "int_converter Test 1" >:: (fun _ ->
        assert_equal 4 (List.length (pick_cards 4)));
  ]

let james = {id = 0; name = "James"; cards = []; money = 32; wins = 1;
             losses = 1; avatar_id = 1; orig_id = 1; 
             consecutive_wins = 0}
let bob = {id = 1; name = "Bob"; cards = [(Hearts, Five)]; money = 23;
           wins = 2; losses = 0; avatar_id = 0; orig_id = 0; 
           consecutive_wins = 2}
let julian = {id = 2; name = "Julian"; cards = [(Diamonds, Five)]; 
              money = 35; wins = 1; losses = 0; avatar_id = 10; orig_id = 2;
              consecutive_wins = 1}

let player_tests =
  [
    "ID Test1" >:: (fun _->
        assert_equal 0  (id james));
    "ID Test2" >:: (fun _->
        assert_equal 1  (id bob));
    "ID Test3" >:: (fun _->
        assert_equal 2  (id julian));
    "Name Test1" >:: (fun _->
        assert_equal "James"  (name james));
    "Name Test2" >:: (fun _->
        assert_equal "Bob"  (name bob));
    "Name Test3" >:: (fun _->
        assert_equal "Julian"  (name julian));
    "Card Test1" >:: (fun _->
        assert_equal  [(Diamonds, Five)] (cards julian));
    "Card Test2" >:: (fun _->
        assert_equal  [] (cards james));
    "Card Test3" >:: (fun _->
        assert_equal  [(Hearts, Five)] (cards bob));
    "Money Test1" >:: (fun _->
        assert_equal  35 (money julian));
    "Money Test2" >:: (fun _->
        assert_equal  23 (money bob));
    "Money Test3" >:: (fun _->
        assert_equal  32 (money james));
    "Avatar Test1" >:: (fun _->
        assert_equal  1 (avatar_id james));
    "Avatar Test2" >:: (fun _->
        assert_equal  0 (avatar_id bob));
    "Avatar Test3" >:: (fun _->
        assert_equal  10 (avatar_id julian));
    "Orig_idTest1" >:: (fun _->
        assert_equal  1 (orig_id james));
    "Orig_idTest2" >:: (fun _->
        assert_equal  0 (orig_id bob));
    "Orig_idTest3" >:: (fun _->
        assert_equal  2 (orig_id julian));
    "ConsecutiveTest1" >:: (fun _->
        assert_equal  0 (consec_wins james));
    "ConsecutiveTest2" >:: (fun _->
        assert_equal  2 (consec_wins bob));
    "ConsecutiveTest3" >:: (fun _->
        assert_equal  1 (consec_wins julian));
    "WinTest1" >:: (fun _->
        assert_equal  1 (wins james));
    "WinTest2" >:: (fun _->
        assert_equal  2 (wins bob));
    "WinTest3" >:: (fun _->
        assert_equal  1 (wins julian));
    "LossTest1" >:: (fun _->
        assert_equal  1 (losses james));
    "LossTest2" >:: (fun _->
        assert_equal  0 (losses bob));
    "LossTest3" >:: (fun _->
        assert_equal  0 (losses julian));
  ]

(* The series of tests for montecarlo simulations. Note that due to
   the random nature of montecarlo, we are unable to make unit tests here
   as our montecarlo functions work  on randomized seed values. *)

let command_tests = [
  "test parse empty" >:: (fun _ -> 
      assert_raises (Empty) (fun () -> parse ""));
  "test parse empty with spaces" >:: (fun _ -> 
      assert_raises (Empty) (fun () -> parse "      "));
  "test parse quit malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "quit l"));
  "test parse quit malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "quit      l"));
  "test parse call malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "call      l"));
  "test parse check malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "check      l"));
  "test parse show malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "show      l"));
  "test parse save malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "save      l"));
  "test parse bet malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "bet  "));
  "test parse raise malformed with spaces" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "raise  "));
  "test parse go malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "go "));
  "test parse go malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "bet "));
  "test parse go malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "raise "));
  "test parse different verb malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "pie clock tower "));
  "test parse take malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "take"));
  "test parse inventory with word malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "inventory chicken"));
  "test parse drop malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "drop"));
  "test parse lock malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "lock"));
  "test parse unlock malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "unlock"));
  "test parse score malformed" >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> parse "score chicken"));
  "test command to string1" >:: (fun _ -> 
      assert_equal "checked!" (command_to_string Check));
  "test command to string2" >:: (fun command -> 
      assert_equal Quit (parse "quit"));
  "test command to string3" >:: (fun command -> 
      assert_equal Check (parse "check"));
  "test command to string4" >:: (fun command -> 
      assert_equal Stack (parse "stack"));
  "test command to string5" >:: (fun command -> 
      assert_equal Fold (parse "fold"));
  "test command to string6" >:: (fun command ->
      assert_equal Call (parse "call"));
  "test command to string7" >:: (fun command ->
      assert_equal Save (parse "save"));
  "test command to string8" >:: (fun command ->
      assert_equal Show (parse "show"));
  "test command to string9" >:: (fun command ->
      assert_equal (Bet 10) (parse "bet 10"));
  "test command to string10" >:: (fun command ->
      assert_equal (Raise 10) (parse "raise 10"));
  "test command to string with extra spaces" >:: (fun command ->
      assert_equal (Raise 10) (parse "raise     10"));
]
(* Tests for the Chen Strength hand analysis function.*)
let hand_analysis_tests =
  [
    "Chen Formula test 1" >:: (fun command ->
        assert_equal 12. (chen_formula [(Spades, Ace);(Spades,King)]));
    "Chen Formula test 2" >:: (fun command ->
        assert_equal 10. (chen_formula [(Spades, Ten);(Diamonds,Ten)]));
    "Chen Formula test 3" >:: (fun command ->
        assert_equal 6. (chen_formula [(Hearts, Five);(Hearts,Seven)]));
    "Chen Formula test 4" >:: (fun command ->
        assert_equal (-1.) (chen_formula [(Spades, Two);(Hearts,Seven)]));
    "Chen Formula test 5" >:: (fun command ->
        assert_equal 20. (chen_formula [(Spades, Ace);(Hearts,Ace)]));
  ]

let table1 = {pot = 0; blind = 1; participants = [james;bob]; board= []}
let james = {id = 0; name = "James"; cards = []; money = 32; wins = 0;
             losses = 0; avatar_id = 0; orig_id = 0; consecutive_wins = 0}
let bob = {id = 1; name = "Bob"; cards = []; money = 32; wins = 0; 
           losses = 0; avatar_id = 0; orig_id = 0; consecutive_wins = 0}
let table1 = {pot = 0; blind = 1; participants = [james;bob]; board= []}
let table2 = deal table1


let table2_players table2 = table2.participants
let james_cards_2 = function
  | {
    id = s;
    name = n;
    cards = c;
    money = m;
  } :: t -> c
  | _ -> failwith "table2 not dealt"

let table1_players = table1.participants
let james_cards = match table1_players with
  | {
    id = s;
    name = n;
    cards = c;
    money = m;
  } :: t -> c
  | _ -> failwith "table2 not dealt"

let empty : Deck.card list = []
let jimmy = {id = 1; name = "Jimmy"; cards = [(Spades, Ace);(Clubs, Ace)];
             money = 32; wins = 0; losses = 0; avatar_id = 0; orig_id = 0;
             consecutive_wins = 0}
let bobby = {id = 2; name = "Bobby"; cards = [(Spades, Two);(Clubs, Two)];
             money = 32; wins = 0; losses = 0; avatar_id = 0; orig_id = 0;
             consecutive_wins = 0}
let alice = {id = 3; name = "Alice"; cards = 
                                       [(Spades, Three);
                                        (Hearts, Four)];
             money = 42; 
             wins = 0; 
             losses = 0; 
             avatar_id = 0; orig_id = 0; consecutive_wins = 0}
let state_table_1 = {pot = 0; blind = 2; participants = [jimmy; bobby; alice];
                     board = [(Hearts, Ace);(Diamonds, Ace);(Spades, King);
                              (Hearts, King); (Hearts, Three)]}

let state_table_a = {pot = 0; blind = 3; participants = [jimmy; bobby];
                     board = [(Hearts, Ace);(Diamonds, Ace);(Spades, King);
                              (Hearts, King); (Hearts, Three)]}
let state_bet_1 =
  {
    bet_player = 1;
    bet_amount = 0;
    bet_paid_amt = [(0,0)];
  }


let state_bet_a =
  {
    bet_player = 1;
    bet_amount = 2;
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
    winners = [(-1,0)];
  }
let statea =
  {
    game_type = 1;
    num_players = 4;
    table = state_table_a;
    player_turn = 0;
    button = 1;
    players_in = [1;2];
    players_played = [];
    bet = state_bet_a;
    avail_action = ["call"];
    winners = [(-1,0)];
  }
let stateb =
  {
    game_type = 1;
    num_players = 2;
    table = state_table_a;
    player_turn = 0;
    button = 1;
    players_in = [1;2];
    players_played = [];
    bet = state_bet_1;
    avail_action = ["fold"];
    winners = [];
  }
let state2 = {state1 with players_in = [2]}
let state3 = {state1 with players_in = [3]}
let state4 = {state1 with players_in = [2; 3]}

let get_state move_result =
  match move_result with
  | Legal t -> t
  | Illegal failed -> failwith failed
let init_players = [{id = 1; name = "Wilson"; cards = []; money = 500;
                     wins = 0; losses = 0; avatar_id = 1; orig_id = 0; 
                     consecutive_wins = 0};
                    {id = 2; name = "Lucy"; cards = []; money = 500; 
                     wins = 0; losses = 0; avatar_id = 2; orig_id = 0; 
                     consecutive_wins = 0}]
let state_1 = (get_avail_action
                 (State.pay_blinds
                    (State.filter_busted_players 
                       {game_type = 0;num_players = 2; table = empty_table;
                        player_turn = 1; button = 1; players_in = [1;2];
                        players_played = [];
                        bet = init_bet [1;2];
                        avail_action = ["bet"; "check"; "fold"];
                        winners = [(-1,0)];
                       } )))
let state_2 = get_state (State.call state_1)
let state_3 = get_state (State.bet_or_raise 50 state_2 "raise")
let state_4 = get_state (State.bet_or_raise 120 state_3 "raise")
let state_5 = get_state (State.call state_4)
let state_6 = get_state (State.check state_5)
let state_7 = get_state (State.bet_or_raise 40 state_6 "bet")

let test_save_1 = State.save "testing1" state1
let test_save_2 = State.save "testing2" state2

(* State Tests for all functions that can be unit tested. 
   The simulation tests
   given below from simulation1 to simulation9 are
   also used to test for the check, call, fold, bet_or_raise,
   calculate_bet_amt and pay_blinds functions present within state.
   Functions that cannot be unit tested or relied on functions that cannot
   be unit tested were tested indirectly via play testing and extensive
   comparisons with existing poker simulators. *)
let init_bet_1 = 
  {
    bet_player = 0;
    bet_amount = 0;
    bet_paid_amt = [(1,0);(2,0);(3,0)]
  }
let init_bet_2 = 
  {
    init_bet_1 with
    bet_paid_amt = [(3,0);(4,0);(1,0);(2,0)]
  }

(* State Tests*)

let state_tests =
  [
    "hand_order_test1" >:: (fun _ ->
        assert_equal [4; 5; 1; 2; 3] (hand_order 5 3 ));
    "hand_order_test2" >:: (fun _ ->
        assert_equal [3; 4; 5; 1; 2] (hand_order 5 2 ));
    "simulation_1" >:: (fun _ ->
        assert_equal 498 (State.find_participant state_1 1).money);
    "simulation_2" >:: (fun _ ->
        assert_equal 495 (State.find_participant state_1 2).money);
    "simulation_3" >:: (fun _ ->
        assert_equal 495 (State.find_participant state_2 1).money);
    "simulation_4" >:: (fun _ ->
        assert_equal 495 (State.find_participant state_3 1).money);
    "simulation_5" >:: (fun _ ->
        assert_equal 445 (State.find_participant state_3 2).money);
    "simulation_6" >:: (fun _ ->
        assert_equal 445 (State.find_participant state_4 2).money);
    "simulation_7" >:: (fun _ ->
        assert_equal 375 (State.find_participant state_4 1).money);
    "simulation_8" >:: (fun _ ->
        assert_equal 375 (State.find_participant state_5 1).money);
    "simulation_9" >:: (fun _ ->
        assert_equal 335 (State.find_participant state_7 2).money);
    "game_type test1" >:: (fun _ ->
        assert_equal 0 (game_type state1));
    "game_type test1" >:: (fun _ ->
        assert_equal 1 (game_type statea));
    "num_players test1" >:: (fun _ ->
        assert_equal 2 (num_players state1));
    "num_players test2" >:: (fun _ ->
        assert_equal 4 (num_players statea));
    "table test1" >:: (fun _ ->
        assert_equal state_table_1 (table state1));
    "table test2" >:: (fun _ ->
        assert_equal state_table_a (table statea));
    "players_in test1" >:: (fun _ ->
        assert_equal [1;2;3] (players_in state1));
    "players_in test2" >:: (fun _ ->
        assert_equal [1;2] (players_in statea));
    "button test1" >:: (fun _ ->
        assert_equal 0 (button state1));
    "button test2" >:: (fun _ ->
        assert_equal 1 (button statea));
    "continue_game_1" >:: (fun _ ->
        assert_equal {statea with winners = []} (continue_game statea));
    "continue_game_2" >:: (fun _ ->
        assert_equal {stateb with winners = []} (continue_game stateb));
    "continue_game_1" >:: (fun _ ->
        assert_equal state_bet_1 (bet state1));
    "continue_game_2" >:: (fun _ ->
        assert_equal state_bet_a (bet statea));
    "avail_actions_1" >:: (fun _ ->
        assert_equal ["fold"] (avail_action state1));
    "avail_actions_2" >:: (fun _ ->
        assert_equal ["call"] (avail_action statea));
    "winner_1" >:: (fun _ ->
        assert_equal [jimmy, 11] (winners state1));
    "winner_2" >:: (fun _ ->
        assert_equal [jimmy, 11] (winners statea));
    "find_stack_1" >:: (fun _ ->
        assert_equal 42 (find_stack 3 state1.table.participants));
    "find_stack_2" >:: (fun _ ->
        assert_equal 32 (find_stack 2 statea.table.participants));
    "init_bet1" >:: (fun _ ->
        assert_equal init_bet_1 (State.init_bet [1;2;3]));
    "init_bet2" >:: (fun _ ->
        assert_equal init_bet_2 (State.init_bet [3;4;1;2]));
    (* test_save saves "testing.json" to current directory *)
    "save1" >:: (fun _ ->
        assert_equal true (Sys.file_exists "testing1.json"));
    "save2" >:: (fun _ ->
        assert_equal true (Sys.file_exists "testing2.json"));
    "load1" >:: (fun _ ->
        assert_equal state1 
          (State.load (Yojson.Basic.from_file "testing1.json")));
    "load2" >:: (fun _ ->
        assert_equal state2
          (State.load (Yojson.Basic.from_file "testing2.json")));
    (* Also extensive testing done by play testing the engine *)
  ]

let table_tests =
  [
    "participants test" >:: (fun _ ->
        assert_equal [{id = 1; name = "Wilson"; cards = []; money = 500;
                       wins = 0; losses = 0; avatar_id = 0; orig_id = 0; 
                       consecutive_wins = 0};
                      {id = 2; name = "Lucy"; cards = []; money = 500;
                       wins = 0; losses = 0; avatar_id = 0; orig_id = 0;
                       consecutive_wins = 0}]
          (participants
             empty_table));
    "deal_test_1" >:: (fun _->
        assert ((deal table1) <> table1));
    "deal_test_2" >:: (fun _->
        assert_equal james_cards empty);
    "deal_failure_test_1" >:: (fun _->
        assert_raises (Failure "player has non 0 cards")
          (fun () -> deal table2));
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

let cards_4 = [(Hearts,Two);(Hearts,Three);(Spades,Four);
               (Diamonds,Five);(Diamonds,Ace);(Diamonds,Ten);(Diamonds,King)]

let cards_a = [1;2;3;4;5;6;7]
let cards_b = [1;2;3;4;5;6;8]
let cards_c = [1;2;3;42;5;6;9]
let cards_d = [1;2;3;4;49;50;51]

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

    "seven_list_eval_test4" >:: (fun _ ->
        assert_equal 1609 (seven_list_eval cards_4));

    "seven_int_list_eval_test1" >:: (fun _ ->
        assert_equal 154 (seven_int_list_eval cards_a));
    "seven_int_list_eval_test2" >:: (fun _ ->
        assert_equal 310 (seven_int_list_eval cards_b));
    "seven_int_list_eval_test3" >:: (fun _ ->
        assert_equal 322 (seven_int_list_eval cards_c));
    "seven_int_list_eval_test4" >:: (fun _ ->
        assert_equal 178 (seven_int_list_eval cards_d));
  ]

let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    table_tests;
    hand_evaluator_tests;
    state_tests;
    player_tests;
    command_tests;
    hand_analysis_tests;
  ]
let _ = run_test_tt_main suite