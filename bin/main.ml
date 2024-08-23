open Dumpmap
open Game

let generation = new_game 800 64
let () = serialize_gen generation |> Yojson.to_file "test_gen.json"
