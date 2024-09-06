open Village
open Mapmanage 
open Mapgen 

(* Vérifie si un noeud est vide *)
let isEmpty = function Vide -> true | _ -> false

(* Test si la ressource 1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le pourcentage est inférieur à la diférence *)
let ingpercent r1 r2 ing pourcent donnee : bool =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  match ing with
  | More ->
      let dif = (nr1 - nr2) * 100 / nr1 in
      if nr1 > nr2 then dif > pourcent else false
  | Less ->
      let dif = (nr2 - nr1) * 100 / nr2 in
      if nr1 < nr2 then dif > pourcent else false

(* Test si la ressource n1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le minimum est inférieur à la diférence *)
let ingflat r1 r2 ing min donnee : bool =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  match ing with
  | More ->
      let dif = nr1 - nr2 in
      if nr1 > nr2 then dif > min else false
  | Less ->
      let dif = nr1 - nr2 in
      if nr1 < nr2 then -dif > min else false

(* Test si la ressource n1 égal à la ressource 2 et si le pourcentage est inférieur à l'écart *)
let equalpercent r1 r2 pourcent donnee =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in
  let som = nr1 + nr2 in
  dif * 100 / som < pourcent

(* Teste si la ressource n1 égal à la ressource 2 et si le minimum est inférieur à l'écart *)
let equalflat r1 r2 min donnee =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in
  let test = if dif < 0 then -dif else dif in
  test < min

(* Effectue le test selon l'objet *)
let test (donnee : data) (condition : condition) : bool =
  match condition with
  | Ingpercent (r1, r2, ing, pourcent) -> ingpercent r1 r2 ing pourcent donnee
  | Ingflat (r1, r2, ing, min) -> ingflat r1 r2 ing min donnee
  | Equalflat (r1, r2, pourcent) -> equalflat r1 r2 pourcent donnee
  | Equalpercent (r1, r2, min) -> equalpercent r1 r2 min donnee

(* Teste s' il y a une tuile du chunk qui est vide *)
let test_not_full (chunk : chunk) : bool =
  match chunk with
  | None -> failwith "Not a Chunk"
  | Chunk (x, _) ->
      let t = ref false in
      for i = 0 to chunk_width - 1 do
        for j = 0 to chunk_width - 1 do
          let (Tile (c, _)) = x.(i).(j) in
          if c == None then t := true
        done
      done;
      !t

(* Ajoute dans un tableau toutes les cases qui sont constructibles *)
let possibilite chunk =
  let arr = Array.make 16 (-1, -1) in
  let tab = get_chunk_tiles chunk in
  let c = ref 0 in
  for i = 0 to chunk_width - 1 do
    for j = 0 to chunk_width - 1 do
      let (Tile (b, _)) = tab.(i).(j) in
      if b = None then arr.(!c) <- (i, j);
      c := !c + 1
    done
  done;
  arr

(* Place le batiment dans un des chunks  *)
let buildtile (build : building) (map : map) (table : (int * int) array) =
  Array.shuffle ~rand:Random.int table;
  let x, y = table.(0) in
  let temp = map.(x).(y) in
  let arr = possibilite temp in
  Array.shuffle ~rand:Random.int arr;
  let rec choice arr c =
    match arr.(c) with
    | -1, -1 -> choice arr (c + 1)
    | i, j -> mutate_building_in_chunk map.(x).(y) (Some build) i j
  in
  choice arr 0

(* Calcule la taille et la position en haut à gauche du tableau *)
let pos_card (pos_list : position list) =
  match pos_list with
  | [] -> failwith "No Chunk"
  | a :: _ ->
      let x, y = a in
      let top, left, right, bot = (ref x, ref y, ref y, ref x) in
      (* corner, largeur, hauteur *)
      let rec parc (pos_list : position list) =
        match pos_list with
        | [] ->
            let b, d, e, g = (!left, !right, !top, !bot) in
            ((e - 1, b - 1), d - b + 3, g - e + 3)
        | (a, b) :: q ->
            if a > !bot then bot := a;
            if a < !top then top := a;
            if b < !left then left := b;
            if b > !right then right := b;
            parc q
      in
      parc pos_list

(* Remplis le tableau avec les positions des chunks *)
let rec proxi (arr : int array array) (pos_list : position list) (world_border:int)
    (corner : position) =
  let p, m = corner in
  (* J'aurais pu mettre [| (-1, -1), (-1, 0), ..., (1, 1) |] *)
  let range = Utils.arr_cartesian_square [| -1; 0; 1 |] in
  match pos_list with
  | [] -> ()
  | (0, _) :: _ | (_, 0) :: _ |(world_border, _) :: _ | (_,world_border) :: _ -> failwith "World limit"
  | (x, y) :: q ->
      for i = 0 to 8 do
        (* Prends successivement les 9 positions adjacentes à (0, 0) *)
        let r_x, r_y = range.(i) in
        if ((r_x, r_y) <> (0, 0)) then
          arr.(x + r_x - p).(y + r_y - m) <- arr.(x + r_x - p).(y + r_y - m) + 1
        else
          arr.(x - p).(y - m) <- -10;
      done;
      proxi arr q world_border corner

(* Parcours la matrice pour lister les positions les plus probables *)
let parc_mat (arr : int array array) (h : int) (l : int) (corner : int * int) =
  let a, b = corner in
  let c = ref 0 in
  let list = ref [] in
  for i = 0 to h - 1 do
    for j = 0 to l - 1 do
      if arr.(i).(j) > !c then (
        list := [ (i, j) ];
        c := arr.(i).(j))
      else if arr.(i).(j) = !c then list := (i + a, j + b) :: !list
      else ()
    done
  done;
  !list

(* Construit le batiment à l'extérieur du village sans biome privilegié *)
let r_buildout (build : building) (map : map) (pos_list : position list) =
  let coner, larg, haut = pos_card pos_list in
  let world_border = Array.length map in 
  let mat = Array.make_matrix haut larg 0 in
  proxi mat pos_list world_border coner;
  let list = parc_mat mat haut larg coner in
  let arr = Array.of_list list in
  buildtile build map arr

(* Construit le batiment à l'intérieur du village sans biome privilegié *)
let r_buildin (build : building) (map : map) (pos_list : position list) =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_not_full map.(x).(y) == false -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  match temp with
  | [] -> r_buildout build map pos_list
  | _ :: _ ->
      let tab = Array.of_list temp in
      buildtile build map tab

(* Classe la liste en deux listes qui regroupe ceux du biome privilégier et les autres dans un autre *)
let classif (list : (int * int) list) (map : map) (biome : biome) =
  let rec parc l1 l2 l3 =
    match l1 with
    | (a, b) :: q when get_chunk_biome map.(a).(b) = biome ->
        parc q ((a, b) :: l2) l3
    | (a, b) :: q -> parc q l2 ((a, b) :: l3)
    | [] -> (l2, l3)
  in
  parc list [] []

(* Construit le batiment à l'extérieur du village avec un biome privilegié *)
let pref_buildout (build : building) (map : map) (pos_list : position list)
    (biome : biome) : unit =
  let corner, larg, haut = pos_card pos_list in
  let world_border = Array.length map in 
  let mat = Array.make_matrix haut larg 0 in
  proxi mat pos_list world_border corner;
  let list = parc_mat mat haut larg corner in
  let pref, autre = classif list map biome in
  match pref with
  | [] ->
      let arr = Array.of_list autre in
      buildtile build map arr
  | _ ->
      let arr = Array.of_list pref in
      buildtile build map arr

(* Construit le batiment à l'intérieur du village avec un biome privilegié *)
let pref_buildin (build : building) (map : map) (pos_list : position list)
    (biome : biome) =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_not_full map.(x).(y) == false -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  let pref, autre = classif temp map biome in
  match pref with
  | [] -> (
      match autre with
      | [] -> r_buildout build map pos_list
      | _ :: _ ->
          let tab = Array.of_list autre in
          buildtile build map tab)
  | _ :: _ ->
      let tab = Array.of_list pref in
      buildtile build map tab

(* Effectue le type de construonction en fonction des paramètres *)
let to_do (action : action) (map : map) (pos_list : position list) : unit =
  let arg, build, pref = action in
  if pref = Random then
    match arg with
    | InCity -> r_buildin build map pos_list
    | OutCity -> r_buildout build map pos_list
  else
    match pref with
    | Pref a -> (
        match arg with
        | InCity -> pref_buildin build map pos_list a
        | OutCity -> pref_buildout build map pos_list a)
    | _ -> failwith "No other possibility"

(* Evalue un noeud et fait ce qu'il faut *)
let rec eval_node (node : tree) (ressource : data) (pos_list : position list)
    (map : map) =
  match node with
  | Vide -> failwith "Empty node"
  | Node (cond, sub_tree_left, sub_tree_right, action) ->
      if isEmpty node then to_do action map pos_list
      else if test ressource cond then
        eval_node sub_tree_left ressource pos_list map
      else eval_node sub_tree_right ressource pos_list map

let test (donnee : data) (condition : condition) : bool =
  match condition with
  | Ingpercent (r1, r2, ing, x) -> ingpercent r1 r2 ing x donnee
  | Ingflat (r1, r2, ing, x) -> ingflat r1 r2 ing x donnee
  | Equalflat (r1, r2, x) -> equalflat r1 r2 x donnee
  | Equalpercent (r1, r2, x) -> equalpercent r1 r2 x donnee

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
