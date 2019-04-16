exception Wrong_Input

let rec get_and_read_input expected_output =
  let input = int_of_string (read_line()) in
  if List.mem (input) expected_output then input
  else raise Wrong_Input

let print_string_list string_list =
  List.iter print_endline string_list

let print_int_list = function
  | h :: t ->
    print_int h;
    List.iter (fun x -> print_string ", "; print_int x) t;
    print_newline ()
  | _ -> print_endline "none"

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
  let rec find_participant' target = function
    | [h] -> h
    | h :: t -> if (Player.id h) = target then h else find_participant' target t in
  find_participant' target (Table.participants (State.table st))

let print_current_state st =
  (* print_endline "Game Type : ";
     print_int (State.game_type st);
     print_endline "\nNumber of Players : ";
     print_int (State.num_players st); *)
  print_endline "\nThe board is : ";
  print_int_list (List.map Deck.int_converter (Table.hole_cards (State.table st)));
  print_endline "\nPlayers in: ";
  print_int_list (State.players_in st);
  print_string "Button: ";
  print_int (State.button st);
  print_newline ();
  print_string "Turn: ";
  print_int (State.player_turn st);
  print_string "\nYour hand is: ";
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
      print_current_state st;
      print_string "> ";

      match read_line () with
      | curr_cmd ->
        match Command.parse curr_cmd with
        | exception Command.Malformed ->
          print_endline "Not a valid command.";
          keep_playing st

        | exception Command.Empty ->
          print_endline "Please enter a command";
          keep_playing st

        | Stack ->
          print_endline "look at stack!";
          State.stack st;
          keep_playing st

        | Quit -> exit 0

        | comm ->
          print_endline (Command.command_to_string comm);
          (
            match State.check st with
            | Legal t ->
              keep_playing t
            | Illegal ->
              print_endline "You can't do that right now!";
              keep_playing st
          ) in

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
let main (() : unit) : unit =
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