(* open Yojson.Safe *)
open Mapgen;;
open Dumpmap;;
open Yojson.Safe;;


let map = (gen_map 100 10 10 1);;
let n = Array.length map;;
print_int n;;
map
  |> serialize_map
  |> to_file "map.json";;

for i = 0 to n - 1 do
  for j = 0 to n - 1 do
    assert (not (isNone (map.(i).(j))))
  done
done

(* let matrix = [|
  [| 1; 2; 3; 4; 5 |];
  [| -2; 1; 3; 4; 0 |];
  [| -2; 1; 3; 0; -5 |];
  [| -2; 2; 0; 4; 3 |];
|];; *)

(* print_chunk_z (submatrix matrix (0, 0) 2) *)

