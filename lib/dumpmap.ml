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

let serialize_map map = matrix_to_json_list serialize_chunk map
