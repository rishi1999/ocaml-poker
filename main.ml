open Card
open Hand_evaluator

exception Wrong_Input

let play_game st =
  let rec get_and_read_input expected_output =
  let input = int_of_string (read_line()) in
  if List.mem (input) expected_output then input
  else raise Wrong_Input in

let print_hline () =
  for i = 1 to 100 do
    print_char '-'
  done;
  print_newline (); 
  print_newline () in

let print_intro () =
  print_endline "Tips:";
  print_string "    The player whose turn it is is shown in ";
  ANSITerminal.(print_string [green] "green");
  print_endline ".";
  print_string "    The button is shown in ";
  ANSITerminal.(print_string [red] "red");
  print_endline ".";
  print_newline ();
  ANSITerminal.(print_string [yellow] "LET'S PLAY!");
  print_newline ();
  print_newline ();
  print_newline () in

let print_list func = function
  | h :: t ->
    func h;
    List.iter (fun x -> print_string ", "; func x) t;
    print_newline ()
  | _ -> print_endline "none" in

let print_string_list = print_list print_string in
let print_int_list = print_list print_int in

let print_players_in st =
  let lst = State.players_in st in
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
  ) in

let print_player_bets st =
  let lst = State.bet_paid_amt st in
  let rec helper = function
    | [] -> ()
    | (a,b) :: t -> if b != 0 then(
        print_string "Player ";
        print_int a;
        print_string " has currently paid: $";
        print_int b;
        print_newline ();
        helper t) in
  let sorted = List.sort compare lst in
  helper sorted;
  print_newline () in

let find_participant st target =
  let rec find_participant' target = function
    | [] -> failwith "PLAYER DOES NOT EXIST"
    | h :: t -> if (Player.id h) = target then h
      else find_participant' target t in
  find_participant' target (Table.participants (State.table st)) in

let print_current_state st =
  ANSITerminal.(
    print_string [yellow] "Player ";
    print_string [yellow] (string_of_int (State.player_turn st));
    print_string [yellow] "'s turn"
  );
  print_newline ();
  print_newline ();
  print_endline "Cards on the board: ";
  (Card.card_printer (Table.board (State.table st)));
  print_newline ();
  print_string "Players in: ";
  print_players_in st;
  print_newline ();
  print_endline "Your hand is: ";
  Card.card_printer (Player.cards (find_participant st
                                     (State.player_turn st)));
  print_newline ();
  print_string "You have: $";
  print_int (Player.money
               (find_participant st (State.player_turn st)));
  print_newline ();
  print_newline ();
  print_player_bets st;
  print_string "Available actions: ";
  print_string_list ("quit" :: "stack" :: (State.avail_action st)); in
  
  print_intro ();
  
  let rec keep_playing st =
    let winning_id = State.winning_player st in
    if (fst winning_id) >= 0 then
      let string = "The winner is player " ^ string_of_int (fst winning_id) 
      ^ " with " ^ Hand_evaluator.rank_mapper (snd winning_id) ^ "!" in
      ANSITerminal.(print_string [yellow] string);
      print_newline ();
      print_newline ();
      (* exit 0 *)
      keep_playing (State.continue_game st)
    else
      print_hline ();
    print_current_state st;
    print_newline ();
    ANSITerminal.(print_string [blue] "> ");

    match read_line () with
    | curr_cmd ->
      match Command.parse curr_cmd with
      | exception Command.Malformed ->
        print_newline ();
        print_endline "Not a valid command.";
        keep_playing st

      | exception Command.Empty ->
        print_newline ();
        print_endline "Please enter a command.";
        keep_playing st

      | Quit -> exit 0

      | comm ->
        let func = State.command_to_function comm in
        match func st with
        | Legal t ->
          print_newline ();
          print_endline (Command.command_to_string comm);
          print_newline ();
          keep_playing (State.get_avail_action t)
        | Illegal str->
          print_newline ();
          print_endline str;
          print_newline ();
          keep_playing (State.get_avail_action st)
  in
  keep_playing st

let init_game num_players =
  print_newline ();
  print_endline "Starting stack amount?";
  ANSITerminal.(print_string [blue] "> ");
  let money = read_int () in
  print_newline ();
  print_endline "Blind amount?";
  ANSITerminal.(print_string [blue] "> ");
  let blind = read_int () in
  let st = match num_players with
    | 1 -> State.init_state 1 2 money blind
    | _ -> State.init_state 0 num_players money blind in
  print_newline ();
  print_newline ();
  play_game st

(** [main ()] prompts the user for the number of players,
    then starts the game. *)
let main () =
  print_newline ();
  print_newline ();
  ANSITerminal.(print_string [blue] "Welcome to OCaml Poker.");
  print_newline ();
  print_newline ();
  (
    print_endline "How many (human) players are there?";
    ANSITerminal.(print_string [blue] "> ");

    try
      read_int ()
    with
    | Failure _ ->
      print_newline ();
      print_endline "Please do not make a mockery of the institution \
                     of poker by inputting such values.";
      print_newline ();
      print_endline "Shutting down....";
      exit 0
  )

  |> init_game


(* Execute the game engine. *)
let () = main ()