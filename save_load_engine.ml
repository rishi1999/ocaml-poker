open Yojson.Basic.Util

let save st = 
Yojson.to_file st

let load json = 
  (* let keys_of_json json = {
    key_id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
    target_room_id = json |> member "target room" |> to_string;
    start_room_description =
      json |> member "start room description" |> to_string;
  } in

  let exit_of_json json = {
    exit_name = json |> member "name" |> to_string;
    room_id = json |> member "room id" |> to_string;
    locked = json |> member "locked" |> to_bool;
  } in

  let items_of_json json = {
    item_name = json |> member "id" |> to_string;
    pickup_points = json |> member "pickup points" |> to_int;
    deliver_points = json |> member "deliver points" |> to_int;
    item_start = json |> member "start room" |> to_string;
    description = json |> member "description" |> to_string;
    start_room_description = 
      json |> member "start room description" |> to_string;
    in_room_description = json |> member "in room description" |> to_string;
    correct_drop_description = 
      json |> member "correct drop description" |> to_string;
  }
  in

  let room_of_json json = {
    id = json |> member "name" |> to_string;
    first_visit_description = 
      json |> member "first visit description" |> to_string;
    subsequent_visit_description = 
      json |> member "subsequent visit description" |> to_string;
    room_with_item_removed_description = 
      json |> member "item removed description" |> to_string;
    points = json |> member "points" |> to_int;
    exits = json |> member "exits" |> to_list |> List.map exit_of_json;
    items = json |> member "items" |> to_list |> List.map items_of_json;
    keys = json |> member "keys" |> to_list |> List.map keys_of_json;
  } in

  let t_of_json json = {
    start_room = json |> member "start room" |> to_string;
    drop_room = json |> member "drop room" |> to_string;
    rooms = json |> member "rooms" |> to_list |> List.map room_of_json;
    starting_description = json |> member "starting description" |> to_string;
    win_description = json |> member "win description" |> to_string;
  } in

  let parse json = 
    try t_of_json json
    with Type_error (s, _) -> failwith ("Parsing error: " ^ s) in

  parse json *)
  json