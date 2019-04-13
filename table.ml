(*Keeps track of players, dealers, and blinds*)
open Deck
open Seats
open Player


type table = {
  dealer: int;
  blind: int;
  participants: player list;
  hole_cards: (Deck.suit * Deck.rank) list;
}

let next_round_players table = function
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab when blind = List.length participants + 1
    ->
    {
      tab with
      dealer = dealer + 1
    }
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab when blind = List.length participants + 1
    ->
    {
      tab with
      dealer = 1;
      blind = 2;
    }
  | tab -> tab

let deal (table : table) : table =
  Deck.deck_init;
  Deck.shuffle_deck;
  let deal_helper (player:player) = match player with
    | {
      Player.action;
      name;
      cards = c;
      money;
    } when List.length c <> 0 -> failwith "player issue"
    | pl ->
      {
        pl with
        cards = Deck.pick_cards 2;
      }
  in
  let rec deal_to_each players list=
    match players with
    | [] -> list
    | player::t -> deal_to_each t ((deal_helper player)::list)

  in
  match table with
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab
    ->
    {
      tab with
      participants = deal_to_each participants [];
    }

let add_to_hole (table:table) : table = match table with
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } when List.length hole_cards > 3
    -> failwith "too many hole cards"
  |
    {
      dealer;
      blind;
      participants;
      hole_cards = h;
    }
    ->
    {
      dealer;
      blind;
      participants;
      hole_cards = Deck.pick_card :: h;
    }


let rec clear_players (p:player list) list= match p with
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
let rec clear_round table = function
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab
    ->
    {
      tab with
      participants = clear_players participants [];
      hole_cards = [];
    }
