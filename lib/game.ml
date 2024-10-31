open Village
open Mapgen
open Newgen
open Score
open Dumpmap
open Mapmanage
open Decision
open Mutation

let new_generation map_width nb_villages =
  let map = gen_map map_width in
  let roots = gen_village_roots (map_width / chunk_width) nb_villages in
  (map, roots)

(* Make all action in one turn *)
let evolution_par_tour (village : village) (map : map) =
  let temp_logistics = update_all_logistics village.logistics in
  print_string "Population: ";
  print_int (calcul_score village map);
  print_char '\n';
  print_string "Bâtiments: ";
  List.iter
    (fun x ->
      print_building x;
      print_char ' ')
    (get_village_buildings village map);
  print_char '\n';
  let new_logistics =
    lack_of_people temp_logistics village.logistics village.position_list map
  in
  village.logistics <- new_logistics;
  eval_node village.tree map village

let init_logistique () =
  ([ (Bed, 1); (Food, 1); (People, 1); (Stone, 0); (Wood, 0) ], void_data)

let starter_pack (map : map) (pos : position) =
  let x, y = pos in
  mutate_building_in_chunk map map.(x).(y) (Some Farm) 0 0;
  mutate_building_in_chunk map map.(x).(y) (Some House) 0 1

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

let nombre_de_tours_par_simulation = 10

let evalvillage a b =
  for i = 0 to nombre_de_tours_par_simulation do
    print_string "Tour n°";
    print_int i;
    print_char '\n';
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
    let scores_pos = Array.make nb_arbres (0, 0) in
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

let scoring (village : village) (map : map) : int = calcul_score village map

(* Associe une carte et une save pour créer une génération *)
let associer_generation (a : save) (map : map) : generation =
  let arbres, pos_array, evaluation = a in
  (arbres, map, pos_array, evaluation)

let do_genertion tree_tab map pos_array : tree array * evaluation =
  let nb_pos = Array.length pos_array in
  let nb_arbres = Array.length tree_tab in
  (* Un tableau à deux entrées qui donne le score de l'arbre selon sa position *)
  let score_mat = Array.make_matrix nb_pos nb_arbres 0 in
  for i = 0 to nb_pos - 1 do
    for j = 0 to nb_arbres - 1 do
      reset_map map;
      let new_village = createvillage tree_tab.(j) pos_array.(i) map j in
      let evaluated_village = evalvillage new_village map in
      let scoretour = scoring evaluated_village map in
      score_mat.(i).(j) <- scoretour
    done
  done;
  let best_trees_array = selection score_mat tree_tab in
  let mutated_best_trees = mutate best_trees_array 1. in
  (mutated_best_trees, score_mat)

(* nb_trees doit être multiple de 5 *)
let game ?(nb_villages = 2) ?(nb_trees = 25) ?(taille_map = 400) (n : int) =
  let (game_array : save array) =
    Array.make (n + 1)
      ( (* Arbres *)
        Array.make nb_trees Vide,
        (* Tableau qui contient les positions des villages *)
        Array.make nb_villages (-1, -1),
        (* Scores *)
        Array.make_matrix nb_villages nb_trees (-1) )
  in
  (* La première case du tableau ne contient que des arbres aléatoires *)
  game_array.(0) <- (gen_trees nb_trees, [||], [||]);
  for i = 1 to n do
    let trees, _, _ = game_array.(i - 1) in
    let map, pos_arr = new_generation taille_map nb_villages in
    let evolved_tree_tab, tree_scores = do_genertion trees map pos_arr in
    (* Stocke les arbres après évolution, là où ils ont évolués
       et les scores qu'on obtenu ces arbres *)
    game_array.(i) <- (evolved_tree_tab, pos_arr, tree_scores)
    (* Stockage éventuel de la carte générée (pas indispensable) *)
    (* Yojson.to_file (Utils.format_map_name i) (serialize_map map) *)
  done;
  Yojson.to_file "game.json" (serialize_save_array game_array)
