exception Wrong_Input

let rec get_and_read_input expected_output =
  let input = int_of_string (read_line()) in
  if List.mem (input) expected_output then input
  else raise Wrong_Input

let print_list func = function
  | h :: t ->
    func h;
    List.iter (fun x -> print_string ", "; func x) t;
    print_newline ()
  | _ -> print_endline "none"

let print_string_list = print_list print_string
let print_int_list = print_list print_int

let print_players_in st =
  let lst = (State.players_in st) in
  ANSITerminal.(
    List.iter
      (fun x ->
         print_string
           (
             if x = (State.player_turn st) then [green]
             else if x = (State.button st) then [red]
             else [white]
           )
           (string_of_int x);
         print_string [white] " "
      ) lst;
    print_newline ()
  )

let print_player_bets st =
  let lst = State.bet_paid_amt st in
  let rec helper = function
    | [] -> ()
    | (a,b) :: t ->
      print_string "Player ";
      print_int a;
      print_string " has currently paid: $";
      print_int b;
      print_newline ();
      helper t in
  helper lst

let find_participant st target =
  let rec find_participant' target = function
    | [] -> failwith "PLAYER DOES NOT EXIST"
    | h :: t -> if (Player.id h) = target then h
      else find_participant' target t in
  find_participant' target (Table.participants (State.table st))

let print_current_state st =
  print_string "The board is: ";
  print_int_list (List.map Deck.int_converter
                    (Table.board (State.table st)));
  print_string "Players in: ";
  print_players_in st;
  print_string "Your hand is: ";
  print_int_list (List.map Deck.int_converter
                    (Player.cards (find_participant st
                                     (State.player_turn st))));
  print_string "You have: $";
  print_int (Player.money
               (find_participant st (State.player_turn st)));
  print_newline ();
  print_player_bets st;
  print_string "Available actions: ";
  print_string_list ("quit" :: "stack" :: (State.avail_action st));
  print_endline "------------------------------------"

let play_game st =
  (*match State.game_type st with
    | 0 -> print_endline "Starting multiplayer game..."
    | 1 -> print_endline "Starting singleplayer game...";*)
  print_newline ();
  print_string "The player whose turn it is is shown in ";
  ANSITerminal.(print_string [green] "green");
  print_endline ".";
  print_string "The button is shown in ";
  ANSITerminal.(print_string [red] "red");
  print_endline ".";
  print_newline ();

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
        print_endline "Please enter a command.";
        keep_playing st

      | Quit -> exit 0

      | comm ->
        let func = State.command_to_function comm in
        match func st with
        | Legal t ->
          print_endline (Command.command_to_string comm);
          keep_playing (State.get_avail_action t)
        | Illegal ->
          print_endline "You can't do that right now!";
          keep_playing (State.get_avail_action st)
  in

  keep_playing st

let init_game num_players =
  print_endline "Starting stack amount?";
  print_string  "> ";
  (* let money = read_int () in *)
  let money = 500 in
  print_endline "Blind amount?";
  print_string  "> ";
  (* let blind = read_int () in *)
  let blind = 5 in
  let st = match num_players with
    | 1 -> State.init_state 1 2 money blind
    | _ -> State.init_state 0 num_players money blind in
  print_newline ();
  play_game st

(** [main ()] prompts the user for the number of players,
    then starts the game. *)
let main (() : unit) : unit =
  print_newline ();
  print_newline ();
  ANSITerminal.(print_string [blue] "Welcome to OCaml Poker.");
  print_newline ();
  print_newline ();
  print_endline "How many (human) players are there?";
  print_string  "> ";

  match read_int () with
  | exception End_of_file -> ()
  | num -> (init_game num)

(* Execute the game engine. *)
let () = main ()