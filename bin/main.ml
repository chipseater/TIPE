open Mapgen
open Dumpmap
open Village

(* let map = gen_map 100 100 10 3;; *)

let tree =
  Node
    ( Equalflat (Wood, Food, 5),
      Vide,
      Node
        ( Ingpercent (People, Stone, More, 2),
          Vide,
          Vide,
          (InCity, Quarry, Random) ),
      (OutCity, House, Pref Plains) )

let stock = [ (Bed, 5); (Food, 10); (People, 5); (Stone, 1); (Wood, 0) ]
let prod = [ (Bed, 0); (Food, 5); (People, 5); (Stone, 0); (Wood, 0) ]
let logistics = (stock, prod)
let village = (1, tree, logistics, (1, 1), [ (1, 1); (1, 2) ]);;

serialize_village village;;
Yojson.to_file "village.json" (serialize_village village)
