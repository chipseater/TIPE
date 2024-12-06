open Village
open Mapmanage
open Mapgen

(* Vérifie si un noeud est vide *)
let estVide = function Vide -> true | _ -> false

(* Test si la ressource 1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le pourcentage est inférieur à la diférence *)
let inegaliteenpourcentage ressource1 ressource2 ing pourcentage donnee : bool =
  let nb_ressource1 = recherche donnee ressource1 in
  let nb_ressource2 = recherche donnee ressource2 in
  match ing with
  | MorePercent ->
      if nb_ressource1 = 0 then true
      else
        let ratio = nb_ressource2  * 100 / nb_ressource1 in
        if nb_ressource1 > nb_ressource2  then ratio > pourcentage else false
  | LessPercent ->
      if nb_ressource1 = 0 then false
      else
        let ratio = nb_ressource2  * 100 / nb_ressource1 in
        if nb_ressource1 < nb_ressource2  then ratio > pourcentage else false

(* Test si la ressource n1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le minimum est inférieur à la diférence *)
let inegalitebrut ressource1 ressource2 ing min donnee : bool =
  let nb_ressource1 = recherche donnee ressource1 in
  let nb_ressource2  = recherche donnee ressource2 in
  match ing with
  | PlusBrut ->
      let dif = nb_ressource1 - nb_ressource2 in
      if nb_ressource1 > nb_ressource2  then dif > min else false
  | MoinBrut ->
      let dif = nb_ressource1 - nb_ressource2  in
      if nb_ressource1 < nb_ressource2  then -dif > min else false
  | EquivalentBrut -> abs (nb_ressource2  - nb_ressource1 ) < min

(* Effectue le test selon l'objet *)
let test (donnee : donne) (condition : condition) : bool =
  match condition with
  | InegaliteEnPourcentage (ressource1, ressource2, ing, pourcentage) -> inegaliteenpourcentage ressource1 ressource2 ing pourcentage donnee
  | InegaliteBrut (ressource1, ressource2, ing, min) -> inegalitebrut ressource1 ressource2 ing min donnee

(* Teste s' il y a une tuile du troncon qui est vide *)
let test_troncon_pas_plein (troncon : troncon) : bool =
  let troncon_tuiles = get_troncon_tuiles troncon in
  let t = ref false in
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let (Tuile (c, _)) = troncon_tuiles.(i).(j) in
      if c = None then t := true
    done
  done;
  !t

(* Ajoute dans un tableau toutes les cases qui sont constructibles *)
let possibilite troncon =
  let arr = Array.make (taille_troncon * taille_troncon) (-1, -1) in
  let tab = get_troncon_tuiles troncon in
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let (Tuile (b, _)) = tab.(i).(j) in
      if b = None then arr.((4 * i) + j) <- (i, j)
    done
  done;
  arr

(* Place le batiment dans un des troncons  *)
let batimenttuile (batiment : batiment) (carte : carte) (table : (int * int) array)
    (village : village) =
  Array.shuffle ~rand:Random.int table;
  let x, y = table.(0) in
  let temp = carte.(x).(y) in
  village.position_list <- (x, y) :: village.position_list;
  let arr = possibilite temp in
  Array.shuffle ~rand:Random.int arr;
  let rec choice arr c =
    match arr.(c) with
    | -1, -1 -> choice arr (c + 1)
    | i, j -> modifie_batiment_dans_troncon carte carte.(x).(y) (Some batiment) i j
  in
  choice arr 0

let nul table carte =
  let n = Array.length table in
  let o = ref 0 in
  try
    for t = 0 to n - 1 do
      o := t;
      let x, y = table.(t) in
      if test_troncon_pas_plein carte.(x).(y) then raise Exit
    done;
    raise Not_found
  with
  | Exit -> table.(!o)
  | Not_found -> (-1, -1)

(* Place le batiment dans un des troncons  *)
let batiment_tuile_in (batiment : batiment) (carte : carte) (table : (int * int) array) =
  Array.shuffle ~rand:Random.int table;
  let x, y = nul table carte in
  (*A voir*)
  if x = -1 then ()
  else
    let temp = carte.(x).(y) in
    let arr = possibilite temp in
    Array.shuffle ~rand:Random.int arr;
    let rec choice arr c =
      match arr.(c) with
      | -1, -1 -> choice arr (c + 1)
      | i, j -> modifie_batiment_dans_troncon carte carte.(x).(y) (Some batiment) i j
    in
    choice arr 0

(* Calcule la taille et la position en haut à gauche du tableau *)
let pos_card (pos_list : position list) =
  match pos_list with
  | [] -> failwith "No Troncon"
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

let rec proxi (arr : int array array) (pos_list : position list)
    (world_limit : int) (corner : position) =
  let pas_valid n i j = i < 0 || i >= n || j < 0 || j >= n in
  let p, m = corner in
  (* J'aurais pu mettre [| (-1, -1), (-1, 0), ..., (1, 1) |] *)
  let range = Utils.arr_cartesian_square [| -1; 0; 1 |] in
  match pos_list with
  | [] -> ()
  | (x, y) :: q ->
      for i = 0 to 8 do
        (* Prends successivement les 9 positions adjacentes à (0, 0) *)
        let r_x, r_y = range.(i) in
        if pas_valid world_limit (x + r_x) (y + r_y) then
          arr.(x - p + r_x).(y - m + r_y) <- -100
        else if (r_x, r_y) <> (0, 0) then
          arr.(x + r_x - p).(y + r_y - m) <- arr.(x + r_x - p).(y + r_y - m) + 1
        else arr.(x - p).(y - m) <- arr.(x - p).(y - m) - 10
      done;
      proxi arr q world_limit corner

(* Parcours la matrice pour lister les positions les plus probables *)
let parc_mat (arr : int array array) (h : int) (l : int) (corner : int * int)
    (carte : carte) =
  let a, b = corner in
  let c = ref 1 in
  let list = ref [] in
  for i = 0 to h - 1 do
    for j = 0 to l - 1 do
      if arr.(i).(j) > !c && test_troncon_pas_plein carte.(i + a).(j + b) then (
        list := [ (i + a, j + b) ];
        c := arr.(i).(j))
      else if arr.(i).(j) = !c && test_troncon_pas_plein carte.(i + a).(j + b) then
        list := (i + a, j + b) :: !list
      else ()
    done
  done;
  !list

(* Construit le batiment à l'extérieur du village sans biome privilegié *)
let r_batimentout (batiment : batiment) (carte : carte) (pos_list : position list)
    (village : village) =
  let coner, larg, haut = pos_card pos_list in
  let mat = Array.make_matrix haut larg 0 in
  let world_limit = Array.length carte in
  proxi mat pos_list world_limit coner;
  let list = parc_mat mat haut larg coner carte in
  let arr = Array.of_list list in
  batimenttuile batiment carte arr village

(* Construit le batiment à l'intérieur du village sans biome privilegié *)
let r_batimentin (batiment : batiment) (carte : carte) (pos_list : position list)
    (village : village) : unit =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_troncon_pas_plein carte.(x).(y) = true -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  match temp with
  | [] -> r_batimentout batiment carte pos_list village
  | _ :: _ ->
      let tab = Array.of_list temp in
      batiment_tuile_in batiment carte tab

(* Classe la liste en deux listes qui regroupe ceux du biome privilégier et les autres dans un autre *)
let classif (list : (int * int) list) (carte : carte) (biome : biome) =
  let rec parc l1 l2 l3 =
    match l1 with
    | (a, b) :: q
      when get_troncon_biome carte.(a).(b) = biome && test_troncon_pas_plein carte.(a).(b) ->
        parc q ((a, b) :: l2) l3
    | (a, b) :: q when test_troncon_pas_plein carte.(a).(b) -> parc q l2 ((a, b) :: l3)
    | _ :: q -> parc q l2 l3
    | [] -> (l2, l3)
  in
  parc list [] []

(* Construit le batiment à l'extérieur du village avec un biome privilegié *)
let pref_batimentout (batiment : batiment) (carte : carte) (pos_list : position list)
    (biome : biome) village : unit =
  let corner, larg, haut = pos_card pos_list in
  let world_limit = Array.length carte in
  let mat = Array.make_matrix haut larg 0 in
  proxi mat pos_list world_limit corner;
  let list = parc_mat mat haut larg corner carte in
  let pref, autre = classif list carte biome in
  match pref with
  | [] ->
      let arr = Array.of_list autre in
      batimenttuile batiment carte arr village
  | _ ->
      let arr = Array.of_list pref in
      batimenttuile batiment carte arr village

(* Construit le batiment à l'intérieur du village avec un biome privilegié *)
let pref_batimentin (batiment : batiment) (carte : carte) (pos_list : position list)
    (biome : biome) (village : village) =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_troncon_pas_plein carte.(x).(y) = true -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  let pref, autre = classif temp carte biome in
  match pref with
  | [] -> (
      match autre with
      | [] -> pref_batimentout batiment carte pos_list biome village
      | _ :: _ ->
          let tab = Array.of_list autre in
          batiment_tuile_in batiment carte tab)
  | _ :: _ ->
      let tab = Array.of_list pref in
      batiment_tuile_in batiment carte tab

(* Effectue le type de construonction en fonction des paramètres *)
let a_faire (action : action) (carte : carte) (pos_list : position list)
    (village : village) : unit =
  let arg, batiment, pref = action in
  if pref = Random then
    match arg with
    | InCity -> r_batimentin batiment carte pos_list village
    | OutCity -> r_batimentout batiment carte pos_list village
  else
    match pref with
    | Pref a -> (
        match arg with
        | InCity -> pref_batimentin batiment carte pos_list a village
        | OutCity -> pref_batimentout batiment carte pos_list a village)
    | _ -> failwith "No other possibility"

(* Evalue un noeud et fait ce qu'il faut *)
let rec eval_node (node : tree) (carte : carte) (village : village) : unit =
  let ressource, _ = village.logistics in
  let pos_list = village.position_list in
  match node with
  | Vide -> failwith "Empty node"
  | Node (cond, sub_tree_left, sub_tree_right, action) -> 
      let test_v = test ressource cond in 
      if estVide sub_tree_left && test_v then
        a_faire action carte pos_list village
      else if estVide sub_tree_right && not test_v then
        a_faire action carte pos_list village
      else if test_v then eval_node sub_tree_left carte village
      else eval_node sub_tree_right carte village
