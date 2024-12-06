open Mapgen
open Mapmanage
open Village

let tuile_to_json tuile =
  let z = get_tuile_z tuile in
  let batiment = get_tuile_batiment tuile in
  `Assoc [ ("z", `Int z); ("bat", `String (option_batiment_to_string batiment)) ]

(* Convertit un tableau en objet json *)
(* to_json est une fonction qui convertit vers le type json souhaité *)
let array_to_json_list to_json (array : 'a array) =
  `List (Array.to_list (Array.map to_json array))

(* Transforme un tableau bidimentionel en objet json *)
let matrix_to_json_list to_json matrix =
  let n = Array.length matrix in
  let rec listify index =
    if index = n then []
    else array_to_json_list to_json matrix.(index) :: listify (index + 1)
  in
  `List (listify 0)

let serialize_troncon (troncon : troncon) =
  let biome = get_troncon_biome troncon in
  let tuiles = get_troncon_tuiles troncon in
  `Assoc
    [
      ("tuiles", matrix_to_json_list tuile_to_json tuiles);
      ("biome", `String (biome_to_string biome));
    ]

let serialize_inegalite_brut inequality =
  match inequality with
  | Village.PlusBrut -> `String "MF"
  | MoinBrut -> `String "LF"
  | EquivalentBrut -> `String "LF"

let serialize_percent_ing inequality =
  match inequality with
  | MorePercent -> `String "MP"
  | LessPercent -> `String "LP"

let serialize_argument argument =
  match argument with InCity -> `String "In" | OutCity -> `String "Out"

let serialize_prio prio =
  match prio with
  | Random -> `String "RND"
  | Pref biome -> `String (biome_to_string biome)

let serialize_batiment batiment =
  match batiment with
  | Maison -> `String "M"
  | Carriere -> `String "Ca"
  | Scierie -> `String "S"
  | Ferme -> `String "F"

let serialize_action action =
  let arg, batiment, prio = action in
  `Assoc
    [
      ("argument", serialize_argument arg);
      ("bat", serialize_batiment batiment);
      ("prio", serialize_prio prio);
    ]

(* type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed *)
let serialize_ressource ressource =
  match ressource with
  | Nouriture -> `String "F"
  | Main_d_oeuvre -> `String "P"
  | Pierre -> `String "S"
  | Wood -> `String "W"
  | Bed -> `String "B"

(* Fonction bien stupide qui renvoie le type de la condition sous forme de string *)
let condition_type_to_string = function
  | InegaliteEnPourcentage (_, _, _, _) -> "InegaliteEnPourcentage"
  | InegaliteBrut (_, _, _, _) -> "InegaliteBrut"

let serialize_condition condition =
  match condition with
  | InegaliteEnPourcentage (rss1, rss2, ing, int) ->
      `Assoc
        [
          ("type", `String (condition_type_to_string condition));
          ("ressource1", serialize_ressource rss1);
          ("ressource2", serialize_ressource rss2);
          ("ing", serialize_percent_ing ing);
          ("int", `Int int);
        ]
  | InegaliteBrut (rss1, rss2, ing, int) ->
      `Assoc
        [
          ("type", `String (condition_type_to_string condition));
          ("ressource1", serialize_ressource rss1);
          ("ressource2", serialize_ressource rss2);
          ("ing", serialize_inegalite_brut ing);
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

let serialize_tree_array tree_array =
  array_to_json_list serialize_tree tree_array

let serialize_pos position =
  let x, y = position in
  `Assoc [ ("x", `Int x); ("y", `Int y) ]

let rec serialize_pos_list pos_list =
  match pos_list with
  | pos :: q -> serialize_pos pos :: serialize_pos_list q
  | [] -> []

let serialize_donne donne =
  let rec donne_to_list = function
    | [] -> []
    | (ressource, qt) :: q ->
        `Assoc
          [
            ("ressource", serialize_ressource ressource); ("quantity", `Int qt);
          ]
        :: donne_to_list q
  in
  `List (donne_to_list donne)

let serialize_logistics logistics =
  let stock, prod = logistics in
  `Assoc [ ("stock", serialize_donne stock); ("prod", serialize_donne prod) ]

let serialize_village (village : village) =
  `Assoc
    [
      ("id", `Int village.id);
      ("tree", serialize_tree village.tree);
      ("logistics", serialize_logistics village.logistics);
      ("position", serialize_pos village.root_position);
      ("pos_list", `List (serialize_pos_list village.position_list));
    ]

let serialize_village_array village_array =
  array_to_json_list serialize_village village_array

let serialize_carte carte = matrix_to_json_list serialize_troncon carte

let serialize_gen generation =
  let villages, carte = generation in
  `Assoc
    [
      ("villages", serialize_village_array villages); ("carte", serialize_carte carte);
    ]

let serialize_game game =
  let rec game_serializer = function
    | [] -> []
    | gen :: q -> serialize_gen gen :: game_serializer q
  in
  `List (game_serializer game)

let serialize_int n = `Int n

let serialize_int_array_array int_array_array =
  matrix_to_json_list serialize_int int_array_array

let serialize_pos_array pos_array = array_to_json_list serialize_pos pos_array

let serialize_save generation =
  let tree_array, pos_array, eval = generation in
  `Assoc
    [
      ("tree_array", serialize_tree_array tree_array);
      ("pos_list", serialize_pos_array pos_array);
      ("evaluation", serialize_int_array_array eval);
    ]

let serialize_save_array tab = array_to_json_list serialize_save tab
