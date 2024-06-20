open Yojson.Safe
open Mapgen
open Dumpmap
open Village
open Decision

let map = gen_map 400 8 10 2;;

let village_exp =
  (1, Vide, (stock_exp, needed_exp), (1, 1), [ (1, 1); (0, 0) ])

