open Player
open ANSITerminal

let win_descrp = [|
  "Winstreak broken.";
  "You are gaining popularity";
  "You are kind of a big deal";
  "Unstoppable";
  "You are a nightmare";
  "If this was a real casino, you would be kicked out";
|]

let dyndescrp player = let index = player.consecutive_wins in
  win_descrp.(index)
