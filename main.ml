open Card
open Hand_evaluator
open Montecarlo
open Avatar


let clear_screen () =
  match Sys.command "clear" with
  | 0 -> ()
  | _ -> exit 2

(*let print_hline () =
  for i = 1 to 100 do
    print_char '-'
  done;
  print_newline ();
  print_newline ()*)

let print_error_message str () =
  print_endline str;
  Unix.sleep 2;
  clear_screen ()

let print_intro () =
  print_endline "Tips:";
  print_string "    The player whose turn it is is shown in ";
  ANSITerminal.(print_string [green] "green");
  print_endline ".";
  print_string "    The button is shown in ";
  ANSITerminal.(print_string [red] "red");
  print_endline ".";
  print_newline ();
  print_newline ();
  ANSITerminal.(print_string [yellow] "LET'S PLAY!");
  print_newline ();
  print_newline ();
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
let print_single_player (st:State.t) num_of_player =
  let player = Table.nth_participant st.table num_of_player in

  ANSITerminal.(
    (fun x ->
       print_string
         (
           let num_spaces = (10 - String.length (Player.name x)) in
           if Player.id x = (State.player_turn st) then [green]
           else if Player.id x = (State.button st) then [red]
           else [default]
         )
         (Player.name x ^ ": $" ^
          (string_of_int (Player.money x) ^ "   "));
    )
  ) player

let print_players_in st =
  let lst = State.players_in st in
  ANSITerminal.(
    List.iter
      (fun x ->
         print_string
           (
             if x = (State.player_turn st) then [green]
             else if x = (State.button st) then [red]
             else [default]
           )
           ((State.find_participant st x).name ^ " ($" ^
            (string_of_int (State.find_stack x st.table.participants)) ^
            ")    ");
      ) lst;
    print_newline ()
  )

let print_table st =
  print_single_player st 0;
  if st.num_players >=3 then print_single_player st 2;
  if st.num_players >=5 then print_single_player st 4;
  if st.num_players >=7 then print_single_player st 6;
  if st.num_players >=9 then print_single_player st 8;
  print_newline ();
  print_endline "________________________________________________________________________";
  print_newline ();
  print_endline "Cards on the board: ";
  (Card.card_printer (Table.board (State.table st)));
  print_newline ();
  print_newline ();
  print_endline "________________________________________________________________________";
  print_newline ();
  if st.num_players >=2 then print_single_player st 1;
  if st.num_players >=4 then print_single_player st 3;
  if st.num_players >=6 then print_single_player st 5;
  if st.num_players >=8 then print_single_player st 7;
  if st.num_players >=10 then print_single_player st 9;
  print_newline ()

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
  (*print_table st;*)
  ANSITerminal.(
    let player = State.find_participant st (State.player_turn st) in
    print_string [yellow] (Player.name player);
    print_string [yellow] "'s turn";
    print_newline ();
    print_newline ();
    print_string [default]
      (avatar_array.(Player.avatar_id player));
    print_newline ();
    print_string [yellow] "Wins: ";
    print_string [yellow] (string_of_int player.wins);
    print_newline ();
    print_string [yellow] "Losses: ";
    print_string [yellow] (string_of_int player.losses);
    print_newline ();
  );
  print_newline ();
  print_newline ();
  print_newline ();
  print_players_in st;
  print_newline ();
  print_player_bets st;
  print_newline ();
  print_string "Available actions: ";
  print_string_list ("quit" :: (State.avail_action st))





let play_game st =
  print_intro ();

  let rec keep_playing st =

    print_table st;
    if (fst (State.winning_player st)) >= 0 then
      (
        clear_screen ();
        print_string "~ ";
        ANSITerminal.(print_string [yellow] "New Game!");
        print_string " ~";
        print_newline ();
        print_newline ();
        keep_playing (State.continue_game st)
      );

    print_current_state st;
    State.prompt "";

    let player = State.(find_participant st (player_turn st)) in

    (* Easy Bot *)
    if (State.game_type st) = 1 && State.player_turn st = 2 then
      if List.mem "check" (State.avail_action st) then
        match State.check st with
        | Legal t ->
          print_newline ();
          print_endline (Command.command_to_string Check);
          print_newline ();
          keep_playing (State.get_avail_action t)
        | Illegal str->
          print_newline ();
          print_endline str;
          print_newline ();
          keep_playing (State.get_avail_action st)
      else if List.mem "call" (State.avail_action st) then
        match (State.call st) with
        | Legal t ->
          print_newline ();
          print_endline (Command.command_to_string Call);
          print_newline ();
          keep_playing (State.get_avail_action t)
        | Illegal str->
          print_newline ();
          print_endline str;
          print_newline ();
          keep_playing (State.get_avail_action st)
      else failwith "AI next move not defined"

    (* Medium Bot *)
    else if (State.game_type st) = 2 && State.player_turn st = 2 then
      let next_action = Montecarlo.declare_action (State.find_participant st 2)
          (Player.cards (State.find_participant st 2)) st 50000 in
      let action = fst next_action in
      print_endline action;
      let amt = snd next_action in
      print_int amt;
      print_newline();
      if action = "raise" then
        match Command.parse (action ^ " " ^ string_of_int amt) with
        | comm ->
          (match State.command_to_function comm st with
           | Legal t ->
             print_newline ();
             print_endline (Command.command_to_string comm);
             print_newline ();
             keep_playing (State.get_avail_action t);
           | Illegal s -> failwith s)
      else
        match Command.parse action with
        | comm ->
          (match State.command_to_function comm st with
           | Legal t ->
             print_newline ();
             print_endline (Command.command_to_string comm);
             print_newline ();
             keep_playing (State.get_avail_action t);
           | Illegal s -> failwith s)
    else
      match
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
             print_string (file_name^".json has been saved!\n");
             keep_playing (State.save file_name st))

        | Show ->
          print_endline "Your hand is: ";
          Card.card_printer (Player.cards (State.find_participant st
                                             (st.player_turn)));
          print_endline "Press Enter to go back to the table";
          ignore (read_line ());
          clear_screen ();
          keep_playing st

        | comm ->
          let func = State.command_to_function comm in
          let move_result = func st in
          match move_result with
          | Legal t ->
            if (fst (State.winning_player st)) < 0 then (
              print_endline (player.name ^ " " ^ Command.command_to_string comm);
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
      ~condition:((fun x -> x >= 20 && x <= 5000), "Min: 20; Max: 5000.") () in
  let blind_max = money / 10 in
  let blind = State.read_integer "Blind amount? ($)"
      ~condition:((fun x -> x >= 2 && x <= blind_max),
                  "Min: 2; Max: " ^ (string_of_int blind_max) ^ ".")
      () in
  let st = match num_players with
    | 1 -> State.prompt "Difficulty of AI? (easy, medium, hard)";
      (
        let game_type = match read_line () with
          | "easy" -> 1
          | "medium" -> 2
          | "hard" -> 3
          | _ -> failwith "ERROR: not a valid difficulty" in
        State.init_state game_type 2 money blind
      )
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

      (file_name ^ ".json") |> Yojson.Basic.from_file |> State.load |> play_game

      (*print_string "Please enter the name of the game file you want to load";
        print_string " without the extension (.json)\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> ()
        | file_name ->
        let extended = file_name ^ ".json" in
        match (Sys.file_exists extended) with
        | false ->
          print_string extended;
          print_endline " is not in current directory!";
          exit 0
        | true ->
          extended |> Yojson.Basic.from_file |> State.load |> play_game*)


    )
  else
    State.read_integer "How many (human) players are there?"
      ~condition:((fun x -> x > 0 && x <= 10), "Min: 1; Max: 10.") ()

    |> init_game

(** [main ()] prompts the user for the number of players,
    then starts the game. *)
let main () =

  print_newline ();
  print_newline ();
  ANSITerminal.(print_string [blue] "Welcome to OCaml Poker.");
  print_newline ();

  State.read_string "Play new game or load game?"
    ~condition:((fun x -> List.mem x ["new"; "load"]), "Options: new, load.") ()
  |> load_or_new


(* Execute the game engine. *)
let () = main ()