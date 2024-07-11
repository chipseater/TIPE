open Mapgen
open Dumpmap

let map = gen_map 100 100 10 3;;

Yojson.to_file "map.json" (serialize_map map)
