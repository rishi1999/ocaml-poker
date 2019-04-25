open Card
open Hand_evaluator
open Montecarlo
open Avatar
open Hand_analysis


let clear_screen () =
  match Sys.command "clear" with
  | 0 -> ()
  | _ -> exit 2

let print_hline length () =
  for i = 1 to length do
    print_char '_'
  done;
  print_newline ()

let print_error_message str () =
  print_endline str;
  Unix.sleep 1;
  clear_screen ()

let print_intro () =
  print_endline "Tips:";
  print_string "    The player whose turn it is is shown in ";
  ANSITerminal.(print_string [green] "green");
  print_endline ".";
  print_string "    Players who are out are shown in ";
  ANSITerminal.(print_string [red] "red");
  print_endline ".";
  print_newline ();
  ANSITerminal.(print_string [yellow] "LET'S PLAY!");
  print_newline ();
  print_endline "make sure to go full screen!";
  print_endline "press enter to continue...";
  ignore (read_line ());
  clear_screen ()

let print_list func = function
  | h :: t ->
    func h;
    List.iter (fun x -> print_string ", "; func x) t;
    print_newline ()
  | _ -> print_endline "none"

let print_string_list = print_list print_string
let print_int_list = print_list print_int

let print_single_player st number_of_players id =
  let table = State.table st in
  let pl = Table.nth_participant
      table
      (
        (
          List.nth (State.hand_order number_of_players st.button) (id - 1)
        ) - 1
      ) in
  ANSITerminal.save_cursor ();
  ANSITerminal.(
    print_string
      (
        if pl.id = (State.player_turn st) then [green]
        else if not (List.mem pl.id (State.players_in st)) then [red]
        else [default]
      )
      (pl.name ^ ": $" ^
       (string_of_int pl.money ^ "   "));
  );
  ANSITerminal.restore_cursor ();
  ANSITerminal.move_cursor 20 0

let print_table st num =

  let print_player_info side =
    let rec print_player_info' count =
      match count with
      | i when i + 1 > num -> ()
      (* note: table printing logic is 0-based, game-wide ids are 1-based,
         so we pass in (i + 1) below. *)
      | i -> print_single_player st num (i + 1);
        print_player_info' (i + 2) in
    ANSITerminal.move_cursor 5 0;
    print_player_info' (
      if side = "top" then 0
      else if side = "bottom" then 1
      else failwith "ERROR: undefined table side"
    );
    print_newline () in

  let print_players_ascii num () =
    let player_ascii = "         ▯▯         " in
    print_string player_ascii;
    for i = 2 to num do
      print_string player_ascii
    done;
    print_newline (); in

  let num_top = (num + 1) / 2 in
  let num_bottom = num - num_top in
  let table_length = max (num_top * 20) 80 in


  print_newline ();
  print_newline ();

  ANSITerminal.set_cursor 5 (-1);
  print_player_info "top";
  ANSITerminal.set_cursor 5 (-1);
  print_hline table_length ();
  ANSITerminal.set_cursor 5 (-1);
  print_players_ascii num_top ();

  print_newline ();
  print_newline ();

  Card.card_printer (Table.board (State.table st));

  print_newline ();
  print_newline ();
  print_newline ();

  let cursor_x = if num_bottom < num_top then 15 else 5 in
  ANSITerminal.set_cursor cursor_x (-1);
  print_players_ascii num_bottom ();
  ANSITerminal.set_cursor 5 (-1);
  print_hline table_length ();
  ANSITerminal.set_cursor cursor_x (-1);
  print_player_info "bottom";

  ANSITerminal.move_cursor 0 (-17);
  for i = 1 to 16 do
    ANSITerminal.set_cursor 5 (-1);
    ANSITerminal.print_string [] "|";
    ANSITerminal.move_cursor (table_length - 1) 0;
    ANSITerminal.print_string [] "|";
    ANSITerminal.set_cursor 5 (-1);
    ANSITerminal.move_cursor 0 1
  done;
  ANSITerminal.move_cursor 0 1


let print_player_bets st =
  let lst = State.bet_paid_amt st in
  let rec helper = function
    | [] -> ()
    | (a,b) :: t -> if b != 0 then
        (
          let p = State.find_participant st a in
          print_string p.name;
          print_string " added $";
          print_int b;
          print_endline " to the pot.";
          helper t
        ) in
  let sorted = List.sort compare lst in
  helper sorted;
  print_newline ()


let print_current_state st =
  ANSITerminal.(
    let player = State.find_participant st (State.player_turn st) in
    print_newline ();
    print_string [yellow] (Player.name player);
    print_string [yellow] "'s turn:";
    print_newline ();

    let col =
      if player.consecutive_wins = 0 then default
      else if player.consecutive_wins = 1 then green
      else if player.consecutive_wins = 2 then cyan
      else magenta in
    print_string [col]
      (avatar_array.(Player.avatar_id player));

    print_newline ();
    print_string [yellow] "Wins: ";
    print_string [yellow] (string_of_int player.wins);
    print_newline ();
    print_string [yellow] "Losses: ";
    print_string [yellow] (string_of_int player.losses);
    print_newline ();
    print_string [yellow] ("$" ^ (string_of_int st.table.pot) ^
                           " is in the pot.");
    print_newline ();
    print_player_bets st;
    print_newline ();
    print_string [default] "Available actions: ";
    print_string_list ("quit" :: (State.avail_action st))
  )

let play_game st =
  print_intro ();

  let rec keep_playing st =

    if (List.length (State.winning_players st)) > 0 then
      (
        clear_screen ();
        print_string "~ ";
        ANSITerminal.(print_string [yellow] "New Game!");
        print_string " ~";
        print_newline ();
        keep_playing (State.continue_game st)
      );

    print_table st st.num_players;
    print_current_state st;
    State.prompt "";

    let player = State.(find_participant st (player_turn st)) in

    (* Medium / Hard Bot *)
    if (State.game_type st = 2 || State.game_type st = 3) &&
       State.player_turn st = 2 then
      let iterations = ref 4000 in
      let change_difficulty game_type =
        if game_type = 2 then iterations := 5000
        else iterations := 15000 in

      print_endline "AI is thinking...";


      change_difficulty (State.game_type st);
      let next_action = Montecarlo.declare_action_2p (State.find_participant
                                                        st 2)
          (Player.cards (State.find_participant st 2)) st !iterations in

      let action = fst next_action in
      let amt = snd next_action in

      if action = "raise" || action = "bet" then
        match Command.parse (action ^ " " ^ string_of_int amt) with
        | comm ->
          (match State.command_to_function comm st with
           | Legal t ->
             clear_screen ();
             if (List.length (State.winning_players st)) = 0 then (
               print_endline (player.name ^ " " ^ Command.command_to_string
                                comm);
               print_newline ()
             );
             keep_playing (State.get_avail_action t);
           | Illegal s -> failwith s)
      else
        match Command.parse action with
        | comm ->
          (match State.command_to_function comm st with
           | Legal t ->
             clear_screen ();
             if (List.length (State.winning_players st)) = 0 then (
               print_endline (player.name ^ " " ^ Command.command_to_string
                                comm);
               print_newline ()
             );
             keep_playing (State.get_avail_action t);
           | Illegal s -> failwith s)
    else
      match
        if State.game_type st = 1 && State.player_turn st = 2 then
          if List.mem "check" (State.avail_action st) then 
            (clear_screen (); "check")
          else if List.mem "call" (State.avail_action st) then 
            (clear_screen (); "call")
          else failwith "ERROR: AI next move not defined"
        else
          let input = read_line () in
          clear_screen ();
          input
      with
      | curr_cmd ->
        match Command.parse curr_cmd with
        | exception Command.Malformed ->
          print_error_message "Not a valid command." ();
          keep_playing st

        | exception Command.Empty ->
          print_error_message "Must enter a command." ();
          keep_playing st

        | Quit -> exit 0

        | Save ->
          print_endline "Please enter the file name.";
          print_string  "> ";
          (match read_line () with
           | exception End_of_file ->
             print_endline "You have not entered a valid name!";
             keep_playing st
           | file_name ->
             print_string (file_name ^ ".json has been saved!\n");
             keep_playing (State.save file_name st))

        | Show ->
          print_endline "Your hand is: ";
          print_newline ();
          print_newline ();
          Card.card_printer (Player.cards (State.find_participant st
                                             (st.player_turn)));
          print_newline ();
          print_newline ();
          print_newline ();
          if List.length (Table.board (State.table st)) = 0 then
            print_endline
              ("The Chen strength of your hand is " ^
               (string_of_int
                  ( int_of_float ((Hand_analysis.chen_formula player.cards) /.
                                  20. *. 100.))) ^
               "%");
          print_newline ();
          print_newline ();
          print_newline ();
          print_endline "press enter to continue...";
          ignore (read_line ());
          clear_screen ();
          keep_playing st

        | comm ->
          let func = State.command_to_function comm in
          let move_result = func st in
          match move_result with
          | Legal t ->
            if (List.length (State.winning_players st)) = 0 then (
              print_endline (player.name ^ " " ^ Command.command_to_string
                               comm);
              print_newline ()
            );
            keep_playing (State.get_avail_action t)

          | Illegal str->
            print_error_message str ();
            keep_playing (State.get_avail_action st)
  in
  keep_playing st

(** [init_game num_players] initializes a game with [num_players] players.
    Requires: integer amount of players [num_players].
    Example: [init_game 3] initializes a game with 3 players. *)
let init_game num_players =
  let money = State.read_integer "Starting stack amount? ($)"
      ~condition:((fun x -> x >= 20 && x <= 5000), "Min: 20; Max: 5000.") ()
  in
  let blind_max = money / 10 in
  let blind = State.read_integer "Blind amount? ($)"
      ~condition:((fun x -> x >= 2 && x <= blind_max),
                  "Min: 2; Max: " ^ (string_of_int blind_max) ^ ".")
      () in
  let st = match num_players with
    | 1 ->
      let difficulty = State.read_string "Difficulty of AI? (easy, medium, \
                                          hard)"
          ~condition:((fun x -> x = "easy" || x = "medium" || x = "hard"),
                      "Options: easy, medium, hard.") () in
      let game_type = match difficulty with
        | "easy" -> 1
        | "medium" -> 2
        | "hard" -> 3
        | _ -> failwith "ERROR: invalid difficulty" in
      State.init_state game_type 2 money blind
    | x when x > 0 -> State.init_state 0 x money blind
    | _ -> failwith "ERROR: negative number of players" in
  clear_screen ();
  play_game st

let load_or_new value =
  if value = "load" then
    (
      let file_name =
        State.read_string
          "Please enter the name of the game file you want to load \
           (without the extension .json)."
          ~condition:((fun x -> Sys.file_exists (x ^ ".json")),
                      "File is not in current directory!") () in

      (file_name ^ ".json") |> Yojson.Basic.from_file |> State.load |>
      play_game
    )
  else
    State.read_integer "How many (human) players are there?"
      ~condition:((fun x -> x > 0 && x <= 10), "Min: 1; Max: 10.") ()

    |> init_game

(** [main ()] prompts the user for the number of players,
    then starts the game. *)
let main () =
  clear_screen ();
  ANSITerminal.(print_string [blue] "Welcome to OCaml Poker.");
  print_newline ();

  State.read_string "Play new game or load game?"
    ~condition:((fun x -> List.mem x ["new"; "load"]), "Options: new, load.")
    ()
  |> load_or_new


(* Execute the game engine. *)
let () = main ()