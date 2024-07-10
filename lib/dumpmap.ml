open Mapgen
open Mapmanage

let tile_to_json tile =
  let z = get_tile_z tile in
  let building = get_tile_building tile in
  `Assoc [ ("z", `Int z); ("building", `String (building_to_string building)) ]

(* Converts an array to a json object *)
(* to_json est une fonction qui convertit vers le type json souhaité *)
let array_to_json_list to_json (array : 'a array) =
  let n = Array.length array in
  (* Converts an array to a list *)
  let rec listify index =
    if index = n then [] else to_json array.(index) :: listify (index + 1)
  in
  `List (listify 0)

(* Converts a 2-dimensional array to a json object *)
let matrix_to_json_list to_json matrix =
  let n = Array.length matrix in
  let rec listify index =
    if index = n then []
    else array_to_json_list to_json matrix.(index) :: listify (index + 1)
  in
  `List (listify 0)

let serialize_chunk (chunk : chunk) =
  let biome = get_chunk_biome chunk in
  let tiles = get_chunk_tiles chunk in
  `Assoc
    [
      ("tiles", matrix_to_json_list tile_to_json tiles);
      ("biome", `String (biome_to_string biome));
    ]

let upscale float_map (max : float) =
  let n = Array.length float_map in
  let new_map = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      new_map.(i).(j) <- int_of_float (max *. float_map.(i).(j))
    done
  done;
  new_map

let print_int_map int_map =
  let n = Array.length int_map in
  for i = 0 to n - 1 do
    for j = 9 to n - 1 do
      print_int int_map.(i).(j);
      print_char ' '
    done;
    print_char '\n'
  done

let serialize_map map = matrix_to_json_list serialize_chunk map
