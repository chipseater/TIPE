open Mapgen
open Dumpmap
open Yojson
;;

let map = gen_map 100 20 10 3;;
print_string (to_string (serialize_map map));;