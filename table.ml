open Deck
open Player

type table = {
  pot: int;
  blind: int;
  participants: Player.player list;
  board: (Deck.suit * Deck.rank) list;
}
let pot tb = tb.pot
let blind tb = tb.blind
let participants tb = tb.participants
let board tb = tb.board

let next_round_players (tab:table) = match tab with
  |
    {
      pot;
      blind;
      participants;
      board;
    } as tab when blind = List.length participants + 1
    ->
    {
      tab with
      blind = 1;
    }
  |
    {

      blind = b;
      participants;
      board;
    } as tab
    ->
    {
      tab with
      blind = b+1;
    }

let deal (table : table) : table =
  Deck.deck_init ();
  Deck.shuffle_deck ();
  let deal_helper (player:player) = match player with
    | {
      id;
      cards = c;
      money;
    } when List.length c <> 0 -> failwith "player has non 0 cards"
    | pl ->
      {
        pl with
        cards = Deck.pick_cards 2;
      }
  in
  let rec deal_to_each players list=
    match players with
    | [] -> list
    | player :: t -> deal_to_each t ((deal_helper player) :: list)

  in
  match table with
  |
    {
      blind;
      participants;
      board;
    } as tab
    ->
    {
      tab with
      participants = deal_to_each participants [];
    }

let add_to_board table =
  match table with
  |
    {
      pot;
      blind;
      participants;
      board;
    } when List.length board > 5
    -> failwith "too many hole cards"
  |
    {
      pot;
      blind;
      participants;
      board = h;
    } when List.length h = 0
    ->
    {
      pot;
      blind;
      participants;
      board = Deck.pick_cards 3 @ h;
    }
  |
    {
      pot;
      blind;
      participants;
      board = h;
    }
    ->
    {
      pot;
      blind;
      participants;
      board = Deck.pick_cards 1 @ h;
    }


let rec clear_players (p:player list) list = match p with
  | [] -> list
  | pl :: t
    -> clear_players t
         (
           {
             pl with
             cards = [];
           } :: list
         )

(** [clear_round table x] is the table with the cards cleared. *)
let rec clear_round table = match table with
  |
    {
      pot;
      blind;
      participants;
      board;
    } as tab
    ->
    {
      tab with
      participants = clear_players participants [];
      board = [];
    }
