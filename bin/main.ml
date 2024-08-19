open Mapgen
open Dumpmap

let map = gen_map ();;

let () = serialize_map map |> Yojson.to_file "map.json"
