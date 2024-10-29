open Village
open Mapgen
open Newgen
open Score
open Dumpmap
open Mapmanage
open Decision

(* A generation binds a map with the villages that live inside this map *)
type score = int array
type evaluation = score array
type generation = tree array * map * position array * evaluation
type save = tree array * position array * evaluation
type game = save array

let new_game map_width nb_villages =
  let map = gen_map map_width in
  let roots = gen_village_roots (map_width / chunk_width) nb_villages in
  (map, roots)

(* let roots, trees = new_game 800 32;; *)

(* Make all action in one turn *)
let evolution_par_tour (village : village) (map : map) =
  let temp_logistics = update_all_logistics village.logistics in
  let new_logistics =
    lack_of_people temp_logistics village.logistics village.position_list map
  in
  village.logistics <- new_logistics;
  eval_node village.tree map village

let init_logistique () = (void_data, void_data)

let starter_pack (map : map) (pos : position) =
  let x, y = pos in
  mutate_building_in_chunk map.(x).(y) (Some Farm) 0 0;
  mutate_building_in_chunk map.(x).(y) (Some House) 0 1

let createvillage (tree : tree) (pos : position) (map : map) (id : int) :
    village =
  (*init le village*)
  starter_pack map pos;
  {
    id;
    tree;
    logistics = init_logistique ();
    root_position = pos;
    position_list = [ pos ];
  }

let nombre_de_tour_par_simulation = 10

let evalvillage a b =
  for _ = 0 to nombre_de_tour_par_simulation do
    evolution_par_tour a b
  done;
  a

let rank n m mat =
  let rg = Array.make m [] in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      let y, x = mat.(i).(j) in
      rg.(y) <- x :: rg.(y)
    done
  done;
  rg

(* Somme les éléments d'une liste *)
let rec somme l = match l with [] -> 0 | e :: q -> e + somme q

let compare_last x y =
  let _, a = x in
  let _, b = y in
  compare a b

let selection score tree_tab =
  (* selectionne les 20 meilleurs *)
  let nb_pos = Array.length score in
  let nb_arbres = Array.length score.(0) in
  let mat = Array.make_matrix nb_pos nb_arbres (0, 0) in
  (* Itère sur chaque position *)
  for i = 0 to nb_pos - 1 do
    let scores_pos = Array.make nb_pos (0, 0) in
    for j = 0 to nb_arbres - 1 do
      scores_pos.(j) <- (j, score.(i).(j))
    done;
    Array.sort compare_last scores_pos;
    mat.(i) <- scores_pos
  done;
  for i = 0 to nb_pos - 1 do
    for j = 0 to nb_arbres - 1 do
      let x, _ = mat.(i).(j) in
      mat.(i).(j) <- (x, nb_arbres - j)
    done
  done;
  let rg = rank nb_pos nb_arbres mat in
  (* Calcul des scores globaux des arbres *)
  let scores_a_trier = Array.make nb_arbres (0, 0) in
  for i = 0 to nb_arbres - 1 do
    let avg_rg = somme rg.(i) / nb_pos in
    (* Stocke l'indice de l'arbre avec son score
       pour le retrouver ensuite *)
    scores_a_trier.(i) <- (i, avg_rg)
  done;
  let () = Array.stable_sort compare_last scores_a_trier in
  (* Retrouve les arbres après tri *)
  let arbres_tries = Array.make (nb_arbres / 5) Vide in
  for i = 0 to (nb_arbres / 5) - 1 do
    let indice_arbre, _ = scores_a_trier.(i) in
    arbres_tries.(i) <- tree_tab.(indice_arbre)
  done;
  arbres_tries

let scoring (village : village) (map : map) : int =
  calcul_score village map

let mutate_trees (tree : tree array) = tree
(* 'creer les 80 autres arbres *)
(* Eric *)

let generaliser (a : save) (map : map) : generation =
  let h1, h2, h3 = a in
  (h1, map, h2, h3)

let do_genertion (generation : generation) : tree array * evaluation =
  let tree_tab, map, pos_array, _ = generation in
  let nb_pos = Array.length pos_array in
  let nb_arbres = Array.length tree_tab in
  let score = Array.make_matrix nb_pos nb_arbres 0 in
  for i = 0 to nb_pos - 1 do
    for j = 0 to nb_arbres - 1 do
      reset_map map;
      let tempvilage = createvillage tree_tab.(j) pos_array.(i) map j in
      let tempvilage = evalvillage tempvilage map in
      let scoretour = scoring tempvilage map in
      score.(i).(j) <- scoretour
    done
  done;
  let new_tree = selection score tree_tab in
  let new_tree = mutate_trees new_tree in
  (new_tree, score)

let game ?(nb_villages = 32) ?(nb_trees = 100) ?(taille_map = 800) (n : int) =
  let (tab : game) =
    Array.make (n + 1)
      ( Array.make nb_trees Vide,
        Array.make nb_villages (-1, -1),
        Array.make_matrix nb_villages nb_trees (-1) )
  in
  (* Init de la première gen d'arbre *)
  let tree_tab1 = gen_trees nb_trees in
  (* Map gen *)
  let map, pos_list = new_game taille_map nb_villages in
  tab.(0) <- (tree_tab1, pos_list, [||]);
  for i = 1 to n - 1 do
    let tree_tab, score = do_genertion (generaliser tab.(i - 1) map) in
    let h1, h2, _ = tab.(i - 1) in
    tab.(i - 1) <- (h1, h2, score);
    (* Map gen *)
    let map, pos_list = new_game taille_map nb_villages in
    Yojson.to_file "efopzvipbaqspivbvqsopvh" (serialize_map map);
    tab.(i) <- (tree_tab, pos_list, [||])
  done;
  Yojson.to_file "game.json" (serialize_save_array tab)
