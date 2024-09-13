open Mapgen
open Mapmanage
open Village
(* open Game *)

let tile_to_json tile =
  let z = get_tile_z tile in
  let building = get_tile_building tile in
  `Assoc [ ("z", `Int z); ("building", `String (building_to_string building)) ]

(* Converts an array to a json object *)
(* to_json est une fonction qui convertit vers le type json souhaité *)
let array_to_json_list to_json (array : 'a array) =
  `List (Array.to_list (Array.map to_json array))

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

let serialize_ing inequality =
  match inequality with
  | More -> `String "M"
  | Less -> `String "L"
  | Equal -> `String "E"

let serialize_argument argument =
  match argument with InCity -> `String "In" | OutCity -> `String "Out"

let serialize_prio prio =
  match prio with
  | Random -> `String "RND"
  | Pref biome -> `String (biome_to_string biome)

let serialize_building building =
  match building with
  | House -> `String "H"
  | Quarry -> `String "Q"
  | Sawmill -> `String "S"
  | Farm -> `String "F"

let serialize_action action =
  let arg, building, prio = action in
  `Assoc
    [
      ("argument", serialize_argument arg);
      ("building", serialize_building building);
      ("prio", serialize_prio prio);
    ]

(* type ressource = Food | People | Stone | Wood | Bed *)
let serialize_ressource ressource =
  match ressource with
  | Food -> `String "F"
  | People -> `String "P"
  | Stone -> `String "S"
  | Wood -> `String "W"
  | Bed -> `String "B"

(* Fonction stupide qui renvoie le type de la condition sous forme de string *)
let condition_type_to_string = function
  | Ingpercent (_, _, _, _) -> "Ingpercent"
  | Ingflat (_, _, _, _) -> "Ingpercent"

let serialize_condition condition =
  match condition with
  | Ingpercent (rss1, rss2, ing, int) | Ingflat (rss1, rss2, ing, int) ->
      `Assoc
        [
          ("type", `String (condition_type_to_string condition));
          ("ressource1", serialize_ressource rss1);
          ("ressource2", serialize_ressource rss2);
          ("ing", serialize_ing ing);
          ("int", `Int int);
        ]

let rec serialize_tree node =
  match node with
  | Vide -> `String "V"
  | Node (cndt, l_child, r_child, action) ->
      `Assoc
        [
          ("condition", serialize_condition cndt);
          ("l_child", serialize_tree l_child);
          ("r_child", serialize_tree r_child);
          ("action", serialize_action action);
        ]

let serialize_pos position =
  let x, y = position in
  `Assoc [ ("x", `Int x); ("y", `Int y) ]

let rec serialize_pos_list pos_list =
  match pos_list with
  | pos :: q -> serialize_pos pos :: serialize_pos_list q
  | [] -> []

let serialize_data data =
  let rec data_to_list = function
    | [] -> []
    | (ressource, qt) :: q ->
        `Assoc
          [
            ("ressource", serialize_ressource ressource); ("quantity", `Int qt);
          ]
        :: data_to_list q
  in
  `List (data_to_list data)

let serialize_logistics logistics =
  let stock, prod = logistics in
  `Assoc [ ("stock", serialize_data stock); ("prod", serialize_data prod) ]

let serialize_village (village : village) =
  let id, tree, logistics, position, pos_list = village in
  `Assoc
    [
      ("id", `Int id);
      ("tree", serialize_tree tree);
      ("logistics", serialize_logistics logistics);
      ("position", serialize_pos position);
      ("pos_list", `List (serialize_pos_list pos_list));
    ]

let serialize_village_array village_array =
  array_to_json_list serialize_village village_array

let serialize_map map = matrix_to_json_list serialize_chunk map

let serialize_gen generation =
  let tree, map , pos_list = generation in
  `Assoc
    [
      ("tree", serialize_tree tree); ("map", serialize_map map); ("pos_list", serialize_pos_list pos_list) ;
    ]

let serialize_game game =
  let rec game_serializer = function
    | [] -> []
    | gen :: q -> serialize_gen gen :: game_serializer q
  in
  `List (game_serializer game)
