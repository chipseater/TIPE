open Yojson.Safe
open Mapgen
open Dumpmap

let () = gen_map 1000 8 10 3 
  |> serialize_map
  |> to_file "map.json"
