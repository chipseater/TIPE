open Village
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
  | [], _ :: _ | _ :: _, [] -> failwith "2.Lack ressource"
  | (e, _) :: _, (r, _) :: _ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> []
  | (e, d) :: q, (_, f) :: s -> (e, d * 100 / f) :: get_ratio (q, s)

(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec search (data : data) ressource =
  match data with
  | [] -> failwith "4.Not Defined"
  | (e, x) :: _ when e = ressource -> x
  | _ :: q -> search q ressource

(* Tests if the passed condition is fullfilled *)
let test difference (condition : condition) =
  let needed_percent, inequality, ressource = condition in
  (* Retrieves the ratio between the stocks and the needs *)
  let ressource_ratio = search difference ressource in
  let ratio = ressource_ratio - needed_percent in
  match (ratio, inequality) with
  | x, Lack when x > 0 -> false
  | x, Surplus when x < 0 -> false
  | x, _ -> x > needed_percent

(* Calculate the number of people in the village *)
let calcul_of_people (data : data) : data =
  let food = search data Food in
  let bed = search data Bed in
  let people = search data People in
  if people > bed then
    addition_data data
      [ (Bed, 0); (Food, 0); (People, bed - people); (Stone, 0); (Wood, 0) ]
  else
    let remaining_beds = bed - people in
    if food < remaining_beds then
      addition_data data
        [ (Bed, 0); (Food, -food); (People, food); (Stone, 0); (Wood, 0) ]
    else
      addition_data data
        [
          (Bed, 0);
          (Food, -remaining_beds);
          (People, remaining_beds);
          (Stone, 0);
          (Wood, 0);
        ]

(*  *)
let update_people (logistics : logistics) : logistics =
  match logistics with stock, need -> ((calcul_of_people stock : data), need)

(* Calcul la nouvelle table de data *)
let update_all_logistics (logistics : logistics) =
  let temp_logistics = update_people logistics in
  let new_logistics = update_logistics temp_logistics in
  (new_logistics : logistics)

(* Set None to the tile i j on the chunk x y *)
let set_None_to (map : map) (i : int) (j : int) (x : int) (y : int) : unit =
  let chunk = map.(x).(y) in
  let tile_z = get_tile_z (get_chunk_tiles chunk).(i).(j) in
  (get_chunk_tiles chunk).(i).(j) <- Tile (None, tile_z)

(* Calcule la nouvelle table de donnée en modifiant la map *)
let destroy_build (logistics : logistics) (position_list : position list)
    (map : map) : logistics =
  let temp_logistics = update_people logistics in
  let stoc, _ = temp_logistics in
  let rec parcours_chunk (i : int) (j : int) (chunk : chunk) (stock : data)
      (x : int) (y : int) =
    match (i, j) with
    | i, _ when i = 0 -> void_data
    | i, j when j = 0 -> parcours_chunk (i - 1) chunk_width chunk stock x y
    | i, j ->
        let w = checkup_tile (get_tile chunk).(i - 1).(j - 1) in
        let a = search w People in
        let b = search stock People in
        if a < b then parcours_chunk i (j - 1) chunk (addition_data w stock) x y
        else (
          set_None_to map (i - 1) (j - 1) x y;
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
  let data, need = logistics in
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

let stock_exp : data =
  [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]

let needed_exp : data =
  [ (Bed, 0); (Food, 100); (People, -25); (Stone, 0); (Wood, 0) ]

let mock_chunk1 =
  Chunk
    ( [|
        [|
          Tile (None, 10);
          Tile (None, 11);
          Tile (Some House, 12);
          Tile (None, 13);
        |];
        [|
          Tile (None, 9);
          Tile (None, 10);
          Tile (Some Quarry, 11);
          Tile (None, 12);
        |];
        [|
          Tile (None, 10); Tile (None, 10); Tile (Some Farm, 11); Tile (None, 11);
        |];
        [|
          Tile (None, 11);
          Tile (None, 10);
          Tile (Some House, 12);
          Tile (None, 10);
        |];
      |],
      Forest )

let mock_chunk2 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )

let mock_chunk3 =
  Chunk
    ( [|
        [|
          Tile (None, 10); Tile (Some Farm, 11); Tile (None, 12); Tile (None, 13);
        |];
        [|
          Tile (None, 9);
          Tile (Some Farm, 10);
          Tile (Some House, 11);
          Tile (None, 12);
        |];
        [|
          Tile (None, 10);
          Tile (Some Farm, 10);
          Tile (Some House, 11);
          Tile (None, 11);
        |];
        [|
          Tile (None, 11); Tile (Some Farm, 10); Tile (None, 12); Tile (None, 10);
        |];
      |],
      Forest )

let map =
  [|
    [| mock_chunk1; mock_chunk2; mock_chunk2 |];
    [| mock_chunk2; mock_chunk3; mock_chunk2 |];
    [| mock_chunk2; mock_chunk2; mock_chunk2 |];
  |]

let village_exp : village =
  (1, Vide, (stock_exp, needed_exp), (1, 1), [ (1, 1); (0, 0) ])

let a = destroy_build (stock_exp, needed_exp) [ (1, 1); (0, 0) ] map
