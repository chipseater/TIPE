open Village
open Mapgen
open Dumpmap

let map = gen_map 100 30 10 2;;

Yojson.to_file "map.json" (serialize_map map);;

