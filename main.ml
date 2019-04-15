exception Wrong_Input

let rec get_and_read_input expected_output = 
  let input = int_of_string (read_line()) in
  if List.mem (input) expected_output then input
  else raise Wrong_Input

let print_string_list string_list =
  List.iter print_endline string_list

let print_int_list int_list =
  int_list |> List.map string_of_int |> List.iter print_endline

let print_bet_situation st = 
  let lst = State.bet_paid_amt st in
  let rec helper = function
    | [] -> ()
    | (a,b)::t -> 
      print_string "Player";
      print_int a;
      print_string " has currently paid: ";
      print_int b;
      print_endline "";
      helper t in
  helper lst

let find_participant st target = 
  let rec helper target = function
    | [h] -> h
    | h :: t -> if (Player.id h) = target then h else helper target t in
  helper target (Table.participants (State.table st))

let print_current_state st =
  (* print_endline "Game Type : ";
     print_int (State.game_type st);
     print_endline "\nNumber of Players : ";
     print_int (State.num_players st); *)
  print_endline "\nThe board is : ";
  print_int_list (List.map Deck.int_converter (Table.hole_cards (State.table st)));
  print_endline "\nPlayers in : ";
  print_int_list (State.players_in st);
  print_string "Button ";
  print_int (State.button st);
  print_string "\nTurn : ";
  print_int (State.player_turn st);
  print_endline "\nYour hand is : ";
  print_int_list (List.map Deck.int_converter (Player.cards 
                                                 (find_participant st (State.player_turn st))));
  print_string "You have: ";
  print_int (Player.money 
               (find_participant st (State.player_turn st)));
  print_bet_situation st;
  print_endline "\nAvailable actions : ";
  print_string_list (State.avail_action st);
  print_endline "------------------------------------"

let play_game st = 
  match State.game_type st with
  | 0 -> print_endline "starting multiplayer game";
    print_int_list (State.players_in st);
    exit 0

  | 1 ->  print_endline "starting AI GAME";

    let rec keep_playing st =
      print_string  "> ";
      print_current_state st;

      match read_line () with
      | curr_cmd ->
        match Command.parse curr_cmd with
        | exception Command.Malformed ->
          print_endline "Not a valid command.";
          keep_playing st

        | exception Command.Empty ->
          print_endline "Please enter a command";
          keep_playing st

        | Check ->
          print_endline "checked!";
          (
            match State.check st with
            | Legal t ->
              keep_playing t
            | Illegal ->
              print_endline "You can't check right now!";
              keep_playing st
          )

        | Fold ->
          print_endline "folded!";
          (
            match State.fold st with
            | Legal t ->
              keep_playing t
            | Illegal ->
              print_endline "You can't fold right now!";
              keep_playing st
          )

        | Call ->
          print_endline "called!";
          (
            match State.call st with
            | Legal changed ->
              keep_playing changed
            | Illegal ->
              print_endline "You can't call right now!";
              keep_playing st
          )

        | Bet bet_amount ->
          let bet_amt = bet_amount in
          print_endline "bet: $";
          print_int bet_amt;
          keep_playing st

        | Raise bet_amount  ->
          let bet_amt = bet_amount in
          print_endline "raise: $";
          print_int bet_amt;
          keep_playing st

        | Stack ->
          print_endline "look at stack!";
          keep_playing st

        | Quit -> exit 0 in

    keep_playing st

  | _ -> failwith "Wrong gametype"


let init_multiplayer f =
  print_endline "how many players?";
  let num_players = int_of_string (read_line ()) in
  print_endline "starting stack?";
  let money = int_of_string (read_line ()) in
  print_endline "blinds?";
  let blind = int_of_string (read_line ()) in
  let st = State.init_state 0 num_players money blind in

  play_game st


let init_ai f =
  print_endline "starting stack?";
  (* let money = int_of_string (read_line ()) in *)
  let money = 500 in
  print_endline "blinds?";
  (* let blind = int_of_string (read_line ()) in *)
  let blind = 5 in
  let st = State.init_state 1 2 money blind in

  play_game st

let init_game game_type =
  match int_of_string game_type with
  | 0 -> init_multiplayer 0
  | 1 -> init_ai 1
  | x -> print_endline "Wrong gametype!";
    exit 0

(** [main ()] prompts the user for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Poker Game.\n");
  print_endline "Do you want to play a multiplayer game(0) or against an AI?(1)";
  print_string  "> ";

  (* match read_line () with
     | exception End_of_file -> ()
     | game_type -> (init_game game_type) *)

  init_game (string_of_int 1)

(* Execute the game engine. *)
let () = main ()