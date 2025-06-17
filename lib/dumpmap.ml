open Type
open Mapmanage

let tuile_a_json tuile =
  let z = recup_tuile_z tuile in
  let batiment = recup_tuile_batiment tuile in
  `Assoc [ ("z", `Int z); ("bat", `String (option_batiment_a_string batiment)) ]

(* Convertit un tableau en objet json *)
(* to_json est une fonction qui convertit vers le type json souhaité *)
let array_a_json_list to_json (array : 'a array) =
  `List (Array.to_list (Array.map to_json array))

(* Transforme un tableau bidimentionel en objet json *)
let matrice_a_json_list to_json matrice =
  let n = Array.length matrice in
  let rec listify index =
    if index = n then []
    else array_a_json_list to_json matrice.(index) :: listify (index + 1)
  in
  `List (listify 0)

let serialize_troncon (troncon : troncon) =
  let biome = recup_troncon_biome troncon in
  let tuiles = recup_troncon_tuiles troncon in
  `Assoc
    [
      ("tuiles", matrice_a_json_list tuile_a_json tuiles);
      ("biome", `String (biome_a_string biome));
    ]

let serialize_inegalite_brut inegalite =
  match inegalite with
  | PlusBrut -> `String "PlusBrut"
  | MoinBrut -> `String "MoinBrut"
  | EquivalentBrut -> `String "EquivalentBrut"

let serialize_percent_ing inegalite =
  match inegalite with
  | MorePercent -> `String "MorePercent"
  | LessPercent -> `String "LessPercent"

let serialize_int n = `Int n

let serialize_bool boo =
  match boo with true -> `String "true" | false -> `String "false"

let serialize_batiment batiment =
  match batiment with
  | Maison -> `String "Maison"
  | Carriere -> `String "Carriere"
  | Scierie -> `String "Scierie"
  | Ferme -> `String "Ferme"
  | Puit -> `String "Puit"
  | Auberge -> `String "Auberge"
  | Statue -> `String "Statue"

(* type ressource = Nouriture | Main_d_oeuvre | Pierre | Wood | Bed *)
let serialize_ressource ressource =
  match ressource with
  | Nouriture -> `String "Nouriture"
  | Main_d_oeuvre -> `String "Main_d_oeuvre"
  | Pierre -> `String "Pierre"
  | Wood -> `String "Wood"
  | Bed -> `String "Bed"
  | Bonheur -> `String "Bonheur"

(* Fonction bien stupide qui renvoie le type de la condition sous forme de string *)
let condition_type_a_string = function
  | InegaliteEnPourcentage (_, _, _, _) -> "InegaliteEnPourcentage"
  | InegaliteBrute (_, _, _, _) -> "InegaliteBrute"

let serialize_condition condition =
  match condition with
  | InegaliteEnPourcentage (rss1, rss2, ing, int) ->
      `Assoc
        [
          ("type", `String (condition_type_a_string condition));
          ("ressource1", serialize_ressource rss1);
          ("ressource2", serialize_ressource rss2);
          ("ing", serialize_percent_ing ing);
          ("int", `Int int);
        ]
  | InegaliteBrute (rss1, rss2, ing, int) ->
      `Assoc
        [
          ("type", `String (condition_type_a_string condition));
          ("ressource1", serialize_ressource rss1);
          ("ressource2", serialize_ressource rss2);
          ("ing", serialize_inegalite_brut ing);
          ("int", `Int int);
        ]

let serialize_couple_mod couple =
  let b1, b2, x, boo = couple in
  `Assoc
    [
      ("bat_org", serialize_batiment b1);
      ("bat_comp", serialize_batiment b2);
      ("nb", serialize_int x);
      ("bool", serialize_bool boo);
    ]

let rec serialize_mod_list liste =
  match liste with
  | couple :: q -> serialize_couple_mod couple :: serialize_mod_list q
  | [] -> []

let rec serialize_arbrepos node =
  match node with
  | Nil -> `String "V"
  | Nodi (liste, child) ->
      `Assoc
        [
          ("liste", `List (serialize_mod_list liste));
          ("child", serialize_arbrepos child);
        ]

let rec serialize_arbre node =
  match node with
  | Vide -> `String "V"
  | Node (cndt, l_child, r_child, bat) ->
      `Assoc
        [
          ("condition", serialize_condition cndt);
          ("l_child", serialize_arbre l_child);
          ("r_child", serialize_arbre r_child);
          ("bat", serialize_batiment bat);
        ]

let serialize_arbre_array arbre_array =
  array_a_json_list serialize_arbre arbre_array

let serialize_arbrepos_array arbre_array =
  array_a_json_list serialize_arbrepos arbre_array

let serialize_pos position =
  let x, y = position in
  `Assoc [ ("x", `Int x); ("y", `Int y) ]

let rec serialize_pos_list pos_list =
  match pos_list with
  | pos :: q -> serialize_pos pos :: serialize_pos_list q
  | [] -> []

let serialize_donne donne =
  let rec donne_a_list = function
    | [] -> []
    | (ressource, qt) :: q ->
        `Assoc
          [
            ("ressource", serialize_ressource ressource); ("quantity", `Int qt);
          ]
        :: donne_a_list q
  in
  `List (donne_a_list donne)

let serialize_logistique logistique =
  let stock, prod = logistique in
  `Assoc [ ("stock", serialize_donne stock); ("prod", serialize_donne prod) ]

let serialize_village (village : village) =
  `Assoc
    [
      ("id", `Int village.id);
      ("arbre", serialize_arbre village.arbre);
      ("arbrepos", serialize_arbrepos village.arbrepos);
      ("logistique", serialize_logistique village.logistique);
      ("position", serialize_pos village.position_position);
      ("pos_list", `List (serialize_pos_list village.position_list));
    ]

let serialize_village_array village_array =
  array_a_json_list serialize_village village_array

let serialize_carte carte = matrice_a_json_list serialize_troncon carte

let serialize_gen generation =
  let villages, carte = generation in
  `Assoc
    [
      ("villages", serialize_village_array villages);
      ("carte", serialize_carte carte);
    ]

let serialize_game game =
  let rec game_serializer = function
    | [] -> []
    | gen :: q -> serialize_gen gen :: game_serializer q
  in
  `List (game_serializer game)

let serialize_int_array_array int_array_array =
  matrice_a_json_list serialize_int int_array_array

let serialize_pos_array pos_array = array_a_json_list serialize_pos pos_array

let serialize_save generation =
  let arbre_array, arbrepos_array, pos_array, eval = generation in
  `Assoc
    [
      ("arbre_array", serialize_arbre_array arbre_array);
      ("arbrepos_array", serialize_arbrepos_array arbrepos_array);
      ("pos_list", serialize_pos_array pos_array);
      ("evaluation", serialize_int_array_array eval);
    ]

let serialize_save_array tab = array_a_json_list serialize_save tab
