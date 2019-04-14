exception Wrong_Input

let rec get_and_read_input expected_output = 
  let input = int_of_string (read_line()) in
  if List.mem (input) expected_output then input
  else raise Wrong_Input

let print_string_list string_list =
  List.iter print_endline string_list

let print_int_list int_list =
  int_list |> List.map string_of_int |> List.iter print_endline

(* NOT WORKING *)
let print_card_list card_list = 
  let rec print_helper outstr = function
    | [] -> outstr
    | h::t -> match h with
      | (a, b) -> print_helper (a ^ b ^ outstr) t in 
  print_helper "" card_list

let play_game st = 
  match State.game_type st with
  | 0 -> print_endline "starting multiplayer game";
    print_int_list (State.players_in st);
    exit 0

  | 1 ->  print_endline "starting AI GAME";

    let rec keep_playing st = 
    match read_line () with
      | curr_cmd ->
        match Command.parse curr_cmd with
        | exception Command.Malformed ->
          print_endline "Not a valid command.";
          keep_playing st

        | exception Command.Empty ->
          print_endline "Please enter a command";
          keep_playing st

        | Fold -> 
          print_endline "folded!";
          keep_playing st

        | Call ->
          print_endline "called!";
          keep_playing st

        | Bet bet_amount ->
          let bet_amt = bet_amount in
          print_int bet_amt;
          print_endline "bet!";
          keep_playing st

        | Raise bet_amount  ->
          let bet_amt = bet_amount in
          print_endline "raise!";
          keep_playing st

        | Ante -> 
          print_endline "post ante!";
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

  play_game (State.init_state 0 num_players money blind)


let init_ai f =
  print_endline "starting stack?";
  let money = int_of_string (read_line ()) in
  print_endline "blinds?";
  let blind = int_of_string (read_line ()) in
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

  match read_line () with
  | exception End_of_file -> ()
  | game_type -> (init_game game_type)

(* match get_and_read_input [0; 1] with
   | exception Wrong_Input -> 
   print_endline "Wrong Input! Try again.";
   get_and_read_input [0; 1]
   | x -> let game_type = x in

   if game_type = 1 then
    let num_players = 2 in
   else
    print_endline "How Many Players?";
    match get_and_read_input [0; 1; 2; 3; 4; 5] with
    | exception Wrong_Input ->
      print_endline "Wrong Input! Try again.";
      get_and_read_input [0; 1; 2; 3; 4; 5]
    | x -> let num_players = x in
      play_game game_type num_players *)


(* Execute the game engine. *)
let () = main ()