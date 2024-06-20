open Yojson.Safe
open Mapgen
open Dumpmap
open Decision

let map = gen_map 400 8 10 2;;

map |> serialize_map |> to_file "map1.json"

let village_exp =
  (1, Vide, (stock_exp, needed_exp), (1, 1), [ (1, 1); (0, 0) ])
    destroy_build (stock_exp, needed_exp)
    [ (1, 1); (0, 0) ]
    map map
  |> serialize_map |> to_file "map2.json"
