open Village
open Mapmanage
open Mapgen

(* Create the new logistics *)
let rec update_logistics (logistics : logistics) : logistics =
  match logistics with
  | [], _ :: _ | _ :: _, [] -> failwith "2.Lack ressource"
  | (e, _) :: _, (r, _) :: _ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> ([], [])
  | (e, d) :: q, (_, f) :: s ->
      let new_stock, need = ((e, d + f), (e, f)) in
      let a, b = update_logistics (q, s) in
      (new_stock :: a, need :: b)

(* Create the ratio of all ressources *)
let rec get_ratio (logistics : logistics) : data =
  match logistics with
  | [], _ :: _ | _ :: _, [] -> raise (Invalid_Argument "Logistics tuple malformed (not the same length for both dicts)")
  | (e, _) :: _, (r, _) :: _ when e <> r -> raise (Invalid_Argument "Logistics tuple malformed (Ressources do not match across dicts)")
  | [], [] -> []
  | (e, d) :: q, (_, f) :: s -> (e, d * 100 / f) :: get_ratio (q, s)

(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec search (data : data) ressource =
  match data with
  | [] -> raise (Invalid_Argument "Ressource not found in data dict")
  | (e, x) :: _ when e = ressource -> x
  | _ :: q -> search q ressource


                                                          (* A Changer *)
(* Tests if the passed condition is fullfilled
let test difference (condition : condition) =
  let needed_percent, inequality, ressource,ressource2  = condition in
  (* Retrieves the ratio between the stocks and the needs *)
  let ressource_ratio = search difference ressource in
  let ratio = ressource_ratio - needed_percent in
  match (ratio, inequality) with
  | x, Lack when x > 0 -> false
  | x, Surplus when x < 0 -> false
  | x, _ -> x > needed_percent *)


  let ingpercent r1 r2 ing x donnee:bool =
    let nr1 = search donnee r1 in
    let nr2 = search donnee r2 in
    match ing with
    |More -> begin 
      let dif = (nr1 - nr2)*100/nr1 in
      if nr1 > nr2 then (dif > x) else false
    end
    |Less -> begin 
      let dif = (nr1 - nr2)/nr1 in
      if nr1 < nr2 then (dif > x) else false
    end
  ;;
  let ingflat r1 r2 ing x donnee:bool =
    let nr1 = search donnee r1 in
    let nr2 = search donnee r2 in
    match ing with
    |More -> begin 
      let dif = nr1 - nr2 in
      if nr1 > nr2 then (dif > x) else false
    end
    |Less -> begin 
      let dif = nr1 - nr2 in
      if nr1 < nr2 then (dif > x) else false
    end
  ;;
let equalpercent r1 r2 x donnee =  
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in 
  false
;;
let equalflat r1 r2 x donnee = 
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in 
  let test = if dif < 0 then -dif else dif in 
  test < x
;;

  let test (donnee:data) (condition:condition):bool = 
  match condition with
  |Ingpercent   (r1,r2,ing,x) -> ingpercent r1 r2 ing x donnee
  |Ingflat      (r1,r2,ing,x) -> ingflat r1 r2 ing x donnee
  |Equalflat    (r1,r2,x)     -> equalflat r1 r2 x         
  |Equalpercent (r1,r2,x)     -> equalpercent r1 r2 x 


;;
(* Calculate the number of people in the village *)
let calcul_of_people (data : data) : data =
  let food = search data Food in
  let bed = search data Bed in
  let people = search data People in
  if people > bed then
    sum_data data
      [ (Bed, 0); (Food, 0); (People, bed - people); (Stone, 0); (Wood, 0) ]
  else
    let remaining_beds = bed - people in
    if food < remaining_beds then
      sum_data data
        [ (Bed, 0); (Food, -food); (People, food); (Stone, 0); (Wood, 0) ]
    else
      sum_data data
        [
          (Bed, 0);
          (Food, -remaining_beds);
          (People, remaining_beds);
          (Stone, 0);
          (Wood, 0);
        ]

let update_people (logistics : logistics) : logistics =
  match logistics with stock, need -> ((calcul_of_people stock : data), need)

(* Calcul la nouvelle table de data *)
let update_all_logistics (logistics : logistics) =
  let temp_logistics = update_people logistics in
  let new_logistics = update_logistics temp_logistics in
  (new_logistics : logistics)

(* Set None to the tile i j on the chunk x y *)
(* let set_None_to (map : map) (i : int) (j : int) (x : int) (y : int) : unit =
  let chunk = map.(x).(y) in
  let tile_z = get_tile_z (get_chunk_tiles chunk).(i).(j) in
  (get_chunk_tiles chunk).(i).(j) <- Tile (None, tile_z) *)

(* Calcule la nouvelle table de donnée en modifiant la map *)
(* T'aurais pas moyen de clarifier ta fonction stp ? *)
let destroy_build (logistics : logistics) (position_list : position list)
    (map : map) : logistics =
  let temp_logistics = update_people logistics in
  let stoc, _ = temp_logistics in
  let rec parcours_chunk (i : int) (j : int) (chunk : chunk) (stock : data)
      (x : int) (y : int) =
    (* Stp Sylvain mets une boucle for à la place *)
    match (i, j) with
    | i, _ when i = 0 -> void_data
    | i, j when j = 0 -> parcours_chunk (i - 1) chunk_width chunk stock x y
    | i, j ->
        (* Explicite tes noms de variable stp, j'ai aucune idée de ce que tu veux faire *)
        let w =
          get_production_from_tile (get_chunk_tiles chunk).(i - 1).(j - 1)
        in
        let a = search w People in
        let b = search stock People in
        if a < b then parcours_chunk i (j - 1) chunk (sum_data w stock) x y
        else (
          (* set_None_to map (i - 1) (j - 1) x y; *)
          let chunk = map.(x).(y) in
          mutate_building_in_chunk chunk None i j
          parcours_chunk i j map.(x).(y) stock x y)
  in
  let rec parcours_list (l : position list) (stock : data) =
    match l with
    | [] -> failwith "Invalid Arg d.1"
    | (x, y) :: [] ->
        parcours_chunk chunk_width chunk_width map.(x).(y) (stock : data) x y
    | (x, y) :: q ->
        parcours_list q
          (parcours_chunk chunk_width chunk_width
             map.(x).(y)
             (stock : data)
             x y)
  in
  let _ = parcours_list position_list stoc in
  update_all_logistics logistics

let lack_of_people (logistics : logistics) (old_logistics : logistics)
    (chunk_list : position list) (map : map) =
  let data, _ = logistics in
  if search data People < 0 then destroy_build old_logistics chunk_list map
  else logistics

(* Make all action in one turn *)
(* let evolution_par_tour (village : village) (map : map) =
   let _, tree, logistics, _, chunk_list = village in
   let stock_temp, _ = logistics in
   let logistics1 = (stock_temp, chunk_list_parcour chunk_list map) in
   let temp_logistics = update_all_logistics logistics1 in
   let new_logistics = lack_of_people temp_logistics logistics chunk_list map in

   let a, _ = new_logistics in

   (* let stockpile = match with *)

   (* Do action defined by the node, lack of the implementation of the village *)
   let to_do action = () in

   let rec eval tree =
     match tree with
     | Vide -> failwith "1.Invalid Argument"
     | Node (cond, tree_verif, tree_not_verif, act) -> (
         let is_condition_fulfilled = test ratio cond in
         match (tree_verif, tree_not_verif) with
         | Vide, Vide -> to_do act
         | Vide, a -> if is_condition_fulfilled then to_do act else eval a
         | a, Vide -> if is_condition_fulfilled then eval a else to_do act
         | a, b -> if is_condition_fulfilled then eval a else eval b)
   in
   eval tree
*)
