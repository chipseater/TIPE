open Yojson.Safe
open Mapgen

let print_matrix_list list =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          print_int y;
          print_char ' ')
        x;
      print_char '\n')
    list

let tile_to_json tile =
  let z = get_tile_z tile in
  let building = get_tile_building tile in
  let tile_data =
    `List
      [
        `Assoc [ ("z", `Int z) ];
        `Assoc [ ("building", `String (building_to_string building)) ];
      ]
  in
  `Assoc [ ("tile", tile_data) ]

(* to_json est une fonction qui convertit vers le type json souhaité *)
let array_to_json_list to_json (array : 'a array) =
  let n = Array.length array in
  let rec listify index =
    if index = n then [] else to_json array.(index) :: listify (index + 1)
  in
  `List (listify 0)

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
      ("chunk_z", matrix_to_json_list tile_to_json tiles);
      ("biome", `String (biome_to_string biome));
    ]

let serialize_map map = `List [ matrix_to_json_list serialize_chunk map ]
