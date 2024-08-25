open Mapgen
open Mapmanage

type ressource = Food | People | Stone | Wood | Bed

(* Dictionnaire contenant des ressources et leur quantités *)
type data = (ressource * int) list

(* Contient à la fois les stocks du village et les ressources produites*)
type logistics = data * data
type position = int * int

(* Arbre *)
type ing = More | Less | Equal

(* Action *)
type argument = InCity | OutCity
type prio = Random | Pref of biome
type action = argument * building * prio

(* Différentes conditions *)
type condition =
  | Ingpercent of ressource * ressource * ing * int
  | Ingflat of ressource * ressource * ing * int

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
let rec sum_data (l1 : data) (l2 : data) : data =
  match (l1, l2) with
  | (r1, _) :: _, (r2, _) :: _ when r1 <> r2 ->
      raise (Invalid_argument "Not the same ressource's place")
  | [], [] -> []
  | _, [] | [], _ -> raise (Invalid_argument "Not the same size")
  | (r1, v1) :: q1, (_, v2) :: q2 -> (r1, v1 + v2) :: sum_data q1 q2

(* Renvoie la production de la tuile d'après le batiment qu'il contient *)
let get_production_from_tile (tile : tile) : data =
  match get_tile_building tile with
  | Some House -> house_data_prodution
  | Some Quarry -> quarry_data_prodution
  | Some Farm -> farm_data_prodution
  | Some Sawmill -> sawmill_data_prodution
  | None -> void_data

(* Somme la prodution dans un chunk *)
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
let rec sum_chunk_list_production (chunk_list : position list) (map : map) =
  match chunk_list with
  | (i, j) :: q ->
      let production = sum_chunk_production map.(i).(j) in
      sum_data production (sum_chunk_list_production q map)
  | [] -> void_data

(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec search (data : data) ressource =
  match data with
  | [] -> raise (Invalid_argument "Ressource not found in data dict")
  | (e, x) :: _ when e = ressource -> x
  | _ :: q -> search q ressource

(* Create the new logistics *)
let rec update_logistics (logistics : logistics) : logistics =
  match logistics with
  | [], _ :: _ | _ :: _, [] -> failwith "2.Lack ressource"
  | (e, _) :: _, (r, _) :: _ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> ([], [])
  | (e, d) :: q, (_, f) :: s ->
      let new_stock, prod = ((e, d + f), (e, 0)) in
      let a, b = update_logistics (q, s) in
      (new_stock :: a, prod :: b)

(* Calculate the number of people in the village *)
let calcul_of_people (data : data) : data =
  let food = search data Food in
  let bed = search data Bed in
  let people = search data People in
  if people > bed then
    sum_data data
      [ (Bed, -bed); (Food, 0); (People, bed - people); (Stone, 0); (Wood, 0) ]
  else
    let remaining_beds = bed - people in
    if food < remaining_beds then
      sum_data data
        [ (Bed, -bed); (Food, -food); (People, food); (Stone, 0); (Wood, 0) ]
    else
      sum_data data
        [
          (Bed, -bed);
          (Food, -remaining_beds);
          (People, remaining_beds);
          (Stone, 0);
          (Wood, 0);
        ]

let update_people (logistics : logistics) : logistics =
  match logistics with stock, prod -> ((calcul_of_people stock : data), prod)

(* Calcul la nouvelle table de data *)
let update_all_logistics (logistics : logistics) =
  let temp_logistics = update_people logistics in
  let new_logistics = update_logistics temp_logistics in
  (new_logistics : logistics)

(* Calcule la nouvelle table de donnée en modifiant la map *)
(* Calcule la logistics a chaque tuile et a chaque fois que la
   resource main d'oeuvre devient négative je change la case en none
   et je recalcule la nouvelle table
*)
let destroy_build (logistics : logistics) (position_list : position list)
    (map : map) : logistics =
  let temp_logistics = update_people logistics in
  let stoc, _ = temp_logistics in

let parcours_chunk (chunk : chunk) (stock : data) =
    let people = ref (search stock People) in
    let temp_stock = ref stock in
    for i = 0 to chunk_width - 1 do
      for j = 0 to chunk_width - 1 do
        let tile_data =
          get_production_from_tile (get_chunk_tiles chunk).(i).(j)
        in
        let people_need = search tile_data People in
        if !people > -people_need then (
          people := !people - people_need;
          temp_stock := sum_data !temp_stock tile_data)
        else mutate_building_in_chunk chunk None i j
      done
    done;
    !temp_stock
  in
  let rec parcours_list (l : position list) (stock : data) =
    match l with
    | [] -> failwith "Invalid Arg d.1"
    | (x, y) :: [] -> parcours_chunk map.(x).(y) (stock : data)
    | (x, y) :: q -> parcours_list q (parcours_chunk map.(x).(y) (stock : data))
  in
  let new_prod = parcours_list position_list stoc in
  update_all_logistics (stoc, new_prod)

let lack_of_people (logistics : logistics) (old_logistics : logistics)
    (chunk_list : position list) (map : map) =
  let data, _ = logistics in
  if search data People < 0 then destroy_build old_logistics chunk_list map
  else logistics
