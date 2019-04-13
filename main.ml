open Deck
open State

exception Wrong_Input

(* let rec get_and_read_input expected_output = 
  let input = int_of_string (read_line()) in
  if List.mem (input) expected_output then input
  else raise Wrong_Input *)

(* let print_string_list string_list =
  List.iter print_endline string_list

let play_multi_game f =
  exit 0 *)

let init_multiplayer f =
  match f with
  | 0 -> print_endline "here";
    exit 0
  | _ -> exit 0
  

(* let init_ai =
  let blind = 4 in
  let money = 500 in
  (* State.init_state  *)
  play_ai_game blind money *)

let init_game game_type =
  match int_of_string game_type with
  | 0 -> init_multiplayer 1
  | 1 -> exit 0 
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