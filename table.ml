open Deck
open Player

type table = {
  pot: int;
  blind: int;
  participants: Player.player list;
  board: Deck.card list;
}
let pot tb = tb.pot
let blind tb = tb.blind
let participants tb = tb.participants
let board tb = tb.board
let nth_participant tb n = List.nth (participants tb) n
let next_round_players = function
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
      blind = b + 1;
    }

let deal table =
  Deck.deck_init ();
  Deck.shuffle_deck ();
  let deal_helper = function
    | {
      id;
      name;
      cards = c;
      money;
    } when List.length c <> 0 ->
      failwith "player has non 0 cards"
    | pl ->
      {
        pl with
        cards = Deck.pick_cards 2;
      } in
  let rec deal_to_each list = function
    | [] -> list
    | h :: t -> deal_to_each (deal_helper h :: list) t in
  {
    table with
    participants = deal_to_each [] table.participants;
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

let rec clear_players list = function
  | [] -> list
  | pl :: t
    -> clear_players
         (
           {
             pl with
             cards = [];
           } :: list
         ) t


let clear_round table =
  {
    table with
    pot = 0;
    participants = clear_players [] table.participants;
    board = [];
  }
