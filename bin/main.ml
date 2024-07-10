open Village
open Mapgen
open Dumpmap

let map = gen_map 1000 30 100 3;;

(* let perlin = perlin_map 1000 100 3;; *)
(* upscale perlin 100. |> print_int_map;; *)

Yojson.to_file "map.json" (serialize_map map)
