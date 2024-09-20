open Game
open Dumpmap

let () = new_game 800 8 |> serialize_game |> Yojson.to_file "test_game.json"

