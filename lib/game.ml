open Type 
open Village
open Mapgen
open Newgen
open Score
open Dumpmap
open Mapmanage
open Decision
open Mutation
open Domainslib.Task
(* open Foret *)
open Variable

exception Couille


let nv_generation taille_carte nb_villages =
  let carte = gen_carte taille_carte in
  let roots = gen_village_roots (taille_carte / taille_troncon) nb_villages in
  (carte, roots)

(* Make all action in one turn *)
let evolution_par_tour (village : village) (carte : carte) (test : bool ref) = 
  let nb_batiment_debut = List.length (get_village_batiments village carte) in (
    (* print_char 'c'; *)
  eval_node village.tree carte village test; (*print_char 'p'*));
  let temp_logistique =
    update_all_logistique village.logistique village.position_list carte
  in
  (* print_char 'u'; *)
  let rec aff = function
    | [] -> print_char '\n'
    | (a, b) :: q ->
      print_int a;
      print_char ' ';
      print_int b;
      print_char '\t';
      aff q
  in
  (* aff village.position_list; *)
  try
  let nv_logistique =
    lack_of_main_d_oeuvre temp_logistique (village.logistique) (village.position_list) carte village
  in
  (* print_char 'r'; *)
  
  let nv_logistique = update_main_d_oeuvre nv_logistique in
  (* print_char 'e'; *)
  (* let rec aff = function
       | [] -> print_char '\n'
       | (a, b) :: q ->
           print_int a;
           print_char ' ';
           print_int b;
           print_char '\t';
           aff q
     in
     aff village.position_list;
     print_string "Population: ";
     print_int (calcul_score village carte);
     print_string "Boof: ";
     print_int
       (let a, _ = village.logistique in
        recherche a Nouriture);
     print_char '\n';
     print_string "Bâtiments: ";
     List.iter
       (fun x ->
         print_batiment x;
         print_char ' ')
       (get_village_batiments village carte);
     print_char '\n'; *)
  
     let nb_batiment_fin = List.length (get_village_batiments village carte) in
  (* Printf.printf "Id %d: %d %d\n" village.id nb_batiment_debut nb_batiment_fin; *)
  if nb_batiment_fin > nb_batiment_debut + 1 then raise Couille else
  village.logistique <- nv_logistique 
  with
  |_ -> failwith "paf" 


let init_logistique () =
  ( [ (Bed, 5); (Nouriture, 20); (Main_d_oeuvre, 50); (Pierre, 0); (Wood, 0) ],
    void_donne )

let logistique_pete () =
  ( [ (Bed, 0); (Nouriture, 0); (Main_d_oeuvre, -1); (Pierre, 0); (Wood, 0) ],
    void_donne )

let starter_pack (carte : carte) (pos : position) =
  let x, y = pos in
  (* print_char 'l'; print_int x;print_char ' ';print_int (Array.length carte);print_char ' '; print_int y;print_char ' ';print_int (Array.length carte.(x)); *)
  modifie_batiment_dans_troncon carte carte.(x).(y) (Some Ferme) 0 0 x y ;
  (* print_char 'o'; *)
  modifie_batiment_dans_troncon carte carte.(x).(y) (Some Maison) 0 1 x y
  (* print_char 'l' *)
let createvillage (tree : tree) (treepos:treepos) (pos : position) (carte : carte) (id : int) :
    village =
  (* init le village *)
  (* print_char 't'; *)
  starter_pack carte pos;
  (* print_char 't'; *)
  {
    id = id;
    tree = tree;
    treepos = treepos;
    logistique = init_logistique ();
    root_position = pos;
    position_list = [ pos ];
  }

let  evalvillage village carte : village =
  let test = ref false in
  try
    (* print_int 402; *)
    for _ = 0 to nombre_de_tours_par_simulation do
      test := false;
      evolution_par_tour village carte test
    done; (*print_int 502;*) village
  with
  | Couille ->print_char 'f';
      {
        id = village.id;
        tree = village.tree;
        treepos = village.treepos;
        logistique = logistique_pete ();
        root_position = village.root_position;
        position_list = village.position_list;
      }
  | Bloque -> village

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

let selection score tree_tab treepos_tab =
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
  let arbres_tries = Array.make (nb_arbres / ratio) Vide in
  let arbrespos_tries = Array.make (nb_arbres/ratio) Nil in 
  for i = 0 to (nb_arbres / ratio) - 1 do
    let indice_arbre, _ = scores_a_trier.(i) in
    arbres_tries.(i) <- tree_tab.(indice_arbre);
    arbrespos_tries.(i) <- treepos_tab.(indice_arbre)
  done;
  (arbres_tries,arbrespos_tries)

let scoring (village : village) (carte : carte) : int =
  calcul_score village carte

(* Associe une carte et une save pour créer une génération *)
let associer_generation (a : save) (carte : carte) : generation =
  let arbres, arbrespos, pos_array, evaluation = a in
  (arbres, arbrespos , carte, pos_array, evaluation)

let do_genertion tree_tab treepos_tab carte_de_base pos_array : tree array* treepos array * evaluation = 
  let nb_pos = Array.length pos_array in
  let nb_arbres = Array.length tree_tab in
  assert(Array.length tree_tab = Array.length treepos_tab);
  (* Un tableau à deux entrées qui donne le score de l'arbre selon sa position *)
  let score_mat = Array.make_matrix nb_pos nb_arbres 0 in
  (* print_int (-1); *)
  (**)
  
  
  let generation_pool = setup_pool ~name:"generation_pool" ~num_domains:4 () in
  let run_tree_at_pos i j = 
    (* print_int (-1); *)
    let carte = copier_carte carte_de_base in
    (* print_int (-2); *)
    (* print_char '\n'; print_int (nb_arbres - j); print_char '_'; print_int (nb_pos - i);print_char ' '; *)
    let nv_village = createvillage tree_tab.(j) treepos_tab.(j) pos_array.(i) carte j in
    (* print_int (-3); *)
    let evaluated_village = evalvillage nv_village carte in
    (* print_int (-4); *)
    let scoretour = scoring evaluated_village carte in
    (* print_char '\n';  *)
    (* print_int (Array.length score_mat.(i)); print_char '!'; print_int j; print_char '='; print_int (Array.length score_mat); print_char '!'; print_int i;print_char ' '; *)
    score_mat.(i).(j) <- scoretour
    (* ;print_string "non"; print_char '\t' *)
  in  


  let run_position i =
    begin  
    try    
    (fun () ->
      parallel_for ~start:0 ~finish:(nb_arbres - 1) ~body:(run_tree_at_pos i) generation_pool)
    |> run generation_pool
  with |_-> () 
  end
  in 
  
  (fun () ->
    parallel_for ~start:0 ~finish:(nb_pos - 1) ~body:(run_position) generation_pool)
  |> run generation_pool;
  (**)
  teardown_pool generation_pool;
  try
  (* print_int (-5); *)
  let (best_trees_array,best_treespos_array) = selection score_mat tree_tab treepos_tab in 
  (* print_int (-6); *)
  let mutated_best_trees = mutate best_trees_array p0 in
  (* print_int (-7); *)
  let mutated_best_treespos = mutatepos best_treespos_array p0 in 
  (* print_int (-8); *)
  (mutated_best_trees,mutated_best_treespos, score_mat)
  with
  |_ -> failwith "Multi"

(* nb_trees doit être multiple de 5 *)
let game1 ?(nb_villages = 2) ?(nb_trees = 8) ?(taille_carte = 400) (n : int) =
  let (game_array : save array) =
    Array.make (n + 1)
      ( (* Arbres *)
        Array.make nb_trees Vide,
        Array.make nb_trees Nil,
        (* Tableau qui contient les positions des villages *)
        Array.make nb_villages (-1, -1),
        (* Scores *)
        Array.make_matrix nb_villages nb_trees (-1) )
  in
  (* La première case du tableau ne contient que des arbres aléatoires *)
  game_array.(0) <- (gen_trees nb_trees, gen_treespos nb_trees ,[||], [||]);
  for i = 1 to n do
    print_int i; print_char '\n';flush stdout;
    let trees,treepos , _, _ = game_array.(i - 1) in
    assert(Array.length trees = Array.length treepos);
    let carte, pos_arr = nv_generation taille_carte nb_villages in
    (* print_int (-1); *)
    let evolved_tree_tab,evolved_treepos_tab, tree_scores = do_genertion trees treepos carte pos_arr in
    (* print_int (-1); print_char '\t'; *)
    (* Stocke les arbres après évolution, là où ils ont évolués
       et les scores qu'on obtenu ces arbres *)
    game_array.(i) <- (evolved_tree_tab, evolved_treepos_tab ,pos_arr, tree_scores)
    (* Stockage éventuel de la carte générée (pas indispensable) *)
    (* Yojson.to_file (Utils.ormat_carte_name i) ("dossier/" ^ serialize_carte carte) *)
  done;
  Yojson.to_file "game.json" (serialize_save_array game_array)
(* 
let game2 ?(nb_villages = 2) ?(nb_trees = 20) ?(taille_carte = 200) (n : int) =
  let (game_array : save array) =
    Array.make (n + 1)
      ( Array.make nb_trees Vide,
        Array.make nb_trees Nil,
        Array.make nb_villages (-1, -1),
        Array.make_matrix nb_villages nb_trees (-1) )
  in
  game_array.(0) <- (const (), [||], [||]);
  for i = 1 to n do
    let trees, _, _ = game_array.(i - 1) in
    let carte, pos_arr = nv_generation taille_carte nb_villages in
    let evolved_tree_tab, evolved_treepos_tab ,tree_scores = do_genertion trees treepos carte pos_arr in
    game_array.(i) <- (evolved_tree_tab, evolved_treepos_tab ,pos_arr, tree_scores)
  done;
  Yojson.to_file "game.json" (serialize_save_array game_array) *)



let game i (n : int) = 
  match i with 
  |1 -> game1 n
  (* |2 -> game2 n  *)
  |_ -> ()

