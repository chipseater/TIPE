open Village
open Mapgen
open Newgen
open Score
open Dumpmap
open Mapmanage
open Decision
open Mutation

let nouvel_generation taille_carte nb_villages =
  let carte = gen_carte taille_carte in
  let roots = gen_village_roots (taille_carte / taille_troncon) nb_villages in
  (carte, roots)

(* Make all action in one turn *)
let evolution_par_tour (village : village) (carte : carte) =
  eval_node village.tree carte village;
  let temp_logistics = update_all_logistics village.logistics village.position_list carte  in
  let nouvel_logistics =
    lack_of_main_d_oeuvre temp_logistics village.logistics village.position_list carte
  in
  let nouvel_logistics = update_main_d_oeuvre nouvel_logistics in
  
  let rec aff = function 
  |[] -> print_char '\n'
  |(a,b)::q -> print_int a; print_char ' '; print_int b; print_char '\t'; aff q
  in
  aff village.position_list;
  print_string "Population: ";
  print_int (calcul_score village carte);
  print_string "Boof: ";
  print_int ( let a,_ = village.logistics in recherche a Nouriture);
  print_char '\n';
  print_string "Bâtiments: ";
  List.iter
    (fun x ->
      print_batiment x;
      print_char ' ')
    (get_village_batiments village carte);
  print_char '\n';

  village.logistics <- nouvel_logistics

let init_logistique () =
  ([ (Bed, 5); (Nouriture, 20); (Main_d_oeuvre, 50); (Pierre, 0); (Wood, 0) ], void_donne)

let starter_pack (carte : carte) (pos : position) =
  let x, y = pos in
  modifie_batiment_dans_troncon carte carte.(x).(y) (Some Ferme) 0 0;
  modifie_batiment_dans_troncon carte carte.(x).(y) (Some Maison) 0 1

let createvillage (tree : tree) (pos : position) (carte : carte) (id : int) :
    village =
  (*init le village*)
  starter_pack carte pos;
  {
    id;
    tree;
    logistics = init_logistique ();
    root_position = pos;
    position_list = [ pos ];
  }

let nombre_de_tours_par_simulation = 100

let evalvillage a b =
  for _ = 0 to nombre_de_tours_par_simulation do
    (*
    print_string "Tour n°";
    print_int i;
    print_char '\n';
    *)
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





  
let scoring (village : village) (carte : carte) : int = calcul_score village carte

(* Associe une carte et une save pour créer une génération *)
let associer_generation (a : save) (carte : carte) : generation =
  let arbres, pos_array, evaluation = a in
  (arbres, carte, pos_array, evaluation)









let do_genertion tree_tab carte pos_array : tree array * evaluation =
  let nb_pos = Array.length pos_array in
  let nb_arbres = Array.length tree_tab in
  (* Un tableau à deux entrées qui donne le score de l'arbre selon sa position *)
  let score_mat = Array.make_matrix nb_pos nb_arbres 0 in
  for i = 0 to nb_pos - 1 do
    for j = 0 to nb_arbres - 1 do
      reset_carte carte;
      let nouvel_village = createvillage tree_tab.(j) pos_array.(i) carte j in
      let evaluated_village = evalvillage nouvel_village carte in
      let scoretour = scoring evaluated_village carte in
      score_mat.(i).(j) <- scoretour
    done
  done;
  let best_trees_array = selection score_mat tree_tab in
  let mutated_best_trees = mutate best_trees_array 1. in
  (mutated_best_trees, score_mat)














(* nb_trees doit être multiple de 5 *)
let game ?(nb_villages = 2) ?(nb_trees = 25) ?(taille_carte = 400) (n : int) =
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
    let carte, pos_arr = nouvel_generation taille_carte nb_villages in
    
    let evolved_tree_tab, tree_scores = do_genertion trees carte pos_arr in
    (* Stocke les arbres après évolution, là où ils ont évolués
       et les scores qu'on obtenu ces arbres *)
    game_array.(i) <- (evolved_tree_tab, pos_arr, tree_scores)
    (* Stockage éventuel de la carte générée (pas indispensable) *)
    (* Yojson.to_file (Utils.ormat_carte_name i) ("dossier/" ^ serialize_carte carte) *)
  done;
  Yojson.to_file "game.json" (serialize_save_array game_array)
