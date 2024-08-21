open Mapgen
open Dumpmap
open Newgen

let map_width = 400
let map = gen_map map_width
let villages = new_villages map_width 20
let generation = villages, map

let () = serialize_gen generation |> Yojson.to_file "test_gen.json"
