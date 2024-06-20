open Mapgen

type ressource = Food | People | Stone | Wood | Bed

(* Dictionnaire contenant des ressources et leur quantités *)
type data = (ressource * int) list

(* Contient à la fois les ressources nécessaires et les stocks du village *)
type logistics = data * data
type position = int * int

(* Arbre *)
type ing = Surplus | Lack

(* Soit le type verb sera suprimé, soit il y aura d'autres constructeurs à l'avenir *)
type verb = Build
type action = verb * building
type condition = int * ing * ressource
type tree = Vide | Node of condition * tree * tree * action

(* Id / arbre de décision/ table de ressource 
  / coordonées du centre / liste des chunks du village
*)
type village = int * tree * logistics * position * position list

(* Valeurs globales *)
(* Les productions seront divisés par un certain coeficient dans le futur *)
let void_data : data =
  [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]

(* À équilibrer *)
let house_data_prodution : data =
  [ (Bed, 5); (Food, 0); (People, -1); (Stone, 0); (Wood, 0) ]

let quarry_data_prodution : data =
  [ (Bed, 0); (Food, 0); (People, -20); (Stone, 100); (Wood, 0) ]

let farm_data_prodution : data =
  [ (Bed, 0); (Food, 10); (People, -25); (Stone, 0); (Wood, 0) ]

let sawmill_data_prodution : data =
  [ (Bed, 0); (Food, 0); (People, -10); (Stone, 0); (Wood, 50) ]

(* Fonction *)
(* Additionne deux dictionnaires de ressources *)
let rec sum_data (l1 : data) (l2 : data) =
  match (l1, l2) with
  | (r1, _) :: _, (r2, _) :: _ when r1 != r2 ->
      raise (Invalid_argument "Not the same ressource's place")
  | [], [] -> []
  | _, [] | [], _ -> raise (Invalid_argument "Not the same size")
  | (r1, v1) :: q1, (_, v2) :: q2 -> (r1, v1 + v2) :: sum_data q1 q2

(* Renvoie la production de la tuile d'après le batiment qu'il contient *)
let get_production_from_tile (tile : tile) : data =
  match tile with
  | Tile (None, _) -> void_data
  | Tile (Some e, _) -> (
      match e with
      | House -> house_data_prodution
      | Quarry -> quarry_data_prodution
      | Farm -> farm_data_prodution
      | Sawmill -> sawmill_data_prodution)

let sum_chunk_production chunk =
  let chunk_production = ref void_data in
  for i = 0 to chunk_width - 1 do
    for j = 0 to chunk_width - 1 do
      let tile = (get_chunk_tiles chunk).(i).(j) in
      let tile_production = get_production_from_tile tile in
      chunk_production := sum_data tile_production !chunk_production
    done
  done;
  !chunk_production

(* Sums the production of the chunk contained in the list *)
let rec sum_chunk_list_production (chunk_list: position list) (map : map) =
  match chunk_list with
  | (i, j) :: q ->
      let production = sum_chunk_production map.(i).(j) in
      sum_data production (sum_chunk_list_production q map)
  | [] -> void_data
