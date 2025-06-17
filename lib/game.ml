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
open Variable

exception Err01

let nv_generation taille_carte nb_villages =
  let carte = gen_carte taille_carte in
  let position =
    gen_village_positions (taille_carte / taille_troncon) nb_villages
  in
  (carte, position)

(* Fait un tour *)
let evolution_par_tour (village : village) (carte : carte) (test : bool ref) =
  let nb_batiment_debut = List.length (recup_village_batiments village carte) in
  eval_noeud village.arbre carte village test;
  let temp_logistique =
    mise_a_jour_logistique village.logistique village.position_list carte
  in
  try
    let nv_logistique =
      manque_of_main_d_oeuvre temp_logistique village.logistique
        village.position_list carte village
    in
    let nv_logistique = update_main_d_oeuvre nv_logistique in
    let nb_batiment_fin = List.length (recup_village_batiments village carte) in
    if nb_batiment_fin > nb_batiment_debut + 1 then raise Err01
    else village.logistique <- nv_logistique
  with _ -> failwith "Err02"

let depart (carte : carte) (pos : position) =
  let x, y = pos in
  modifie_batiment_dans_troncon carte carte.(x).(y) (Some Ferme) 0 0 x y;
  modifie_batiment_dans_troncon carte carte.(x).(y) (Some Maison) 0 1 x y

let createvillage (arbre : arbre) (arbrepos : arbrepos) (pos : position)
    (carte : carte) (id : int) : village =
  (* init le village *)
  depart carte pos;
  {
    id;
    arbre;
    arbrepos;
    logistique = init_logistique ();
    position_position = pos;
    position_list = [ pos ];
  }

let evalvillage village carte : village =
  let test = ref false in
  try
    for _ = 0 to nombre_de_tours_par_simulation do
      test := false;
      evolution_par_tour village carte test
    done;
    village
  with
  | Err01 ->
      {
        id = village.id;
        arbre = village.arbre;
        arbrepos = village.arbrepos;
        logistique = logistique_pete ();
        position_position = village.position_position;
        position_list = village.position_list;
      }
  | Bloque -> village

let rang n m mat =
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

let compare_deux x y =
  let _, a = x in
  let _, b = y in
  compare a b

let selection score arbre_tab arbrepos_tab =
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
    Array.sort compare_deux scores_pos;
    mat.(i) <- scores_pos
  done;
  for i = 0 to nb_pos - 1 do
    for j = 0 to nb_arbres - 1 do
      let x, _ = mat.(i).(j) in
      mat.(i).(j) <- (x, nb_arbres - j)
    done
  done;
  let rg = rang nb_pos nb_arbres mat in
  (* Calcul des scores globaux des arbres *)
  let scores_a_trier = Array.make nb_arbres (0, 0) in
  for i = 0 to nb_arbres - 1 do
    let moy_rg = somme rg.(i) / nb_pos in
    (* Stocke l'indice de l'arbre avec son score
       pour le retrouver ensuite *)
    scores_a_trier.(i) <- (i, moy_rg)
  done;
  let () = Array.stable_sort compare_deux scores_a_trier in
  (* Retrouve les arbres après tri *)
  let arbres_tries = Array.make (nb_arbres / ratio) Vide in
  let arbrespos_tries = Array.make (nb_arbres / ratio) Nil in
  for i = 0 to (nb_arbres / ratio) - 1 do
    let indice_arbre, _ = scores_a_trier.(i) in
    arbres_tries.(i) <- arbre_tab.(indice_arbre);
    arbrespos_tries.(i) <- arbrepos_tab.(indice_arbre)
  done;
  (arbres_tries, arbrespos_tries)

let noter (village : village) (carte : carte) : int = calcul_score village carte

(* Associe une carte et une save pour créer une génération *)
let associer_generation (a : save) (carte : carte) : generation =
  let arbres, arbrespos, pos_array, evaluation = a in
  (arbres, arbrespos, carte, pos_array, evaluation)

let fait_genertion arbre_tab arbrepos_tab carte_de_base pos_array :
    arbre array * arbrepos array * evaluation =
  let nb_pos = Array.length pos_array in
  let nb_arbres = Array.length arbre_tab in
  assert (Array.length arbre_tab = Array.length arbrepos_tab);
  (* Un tableau à deux entrées qui donne le score de l'arbre selon sa position *)
  let score_mat = Array.make_matrix nb_pos nb_arbres 0 in
  let generation_pool = setup_pool ~name:"generation_pool" ~num_domains:4 () in
  let run_arbre_at_pos i j =
    let carte = copier_carte carte_de_base in
    let nv_village =
      createvillage arbre_tab.(j) arbrepos_tab.(j) pos_array.(i) carte j
    in
    let evaluated_village = evalvillage nv_village carte in
    let scoretour = noter evaluated_village carte in
    score_mat.(i).(j) <- scoretour
  in
  let run_position i =
    try
      (fun () ->
        parallel_for ~start:0 ~finish:(nb_arbres - 1) ~body:(run_arbre_at_pos i)
          generation_pool)
      |> run generation_pool
    with
    | Invalid_argument x ->
        print_string x;
        print_char '\n'
    | _ -> ()
  in
  (fun () ->
    parallel_for ~start:0 ~finish:(nb_pos - 1) ~body:run_position
      generation_pool)
  |> run generation_pool;
  teardown_pool generation_pool;
  try
    let meilleur_arbres_array, meilleur_arbrespos_array =
      selection score_mat arbre_tab arbrepos_tab
    in
    let mute_meilleur_arbres = mutate meilleur_arbres_array p0 in
    let mute_meilleur_arbrespos = mutatepos meilleur_arbrespos_array p0 in
    (mute_meilleur_arbres, mute_meilleur_arbrespos, score_mat)
  with _ -> failwith "Multi"

let game1 ?(nb_villages = 2) ?(nb_arbres = 2) ?(taille_carte = 40) (n : int) =
  let (game_array : save array) =
    Array.make (n + 1)
      ( (* Arbres *)
        Array.make nb_arbres Vide,
        Array.make nb_arbres Nil,
        (* Tableau qui contient les positions des villages *)
        Array.make nb_villages (-1, -1),
        (* Scores *)
        Array.make_matrix nb_villages nb_arbres (-1) )
  in
  (* La première case du tableau ne contient que des arbres aléatoires *)
  game_array.(0) <- (gen_arbres nb_arbres, gen_arbrespos nb_arbres, [||], [||]);
  for i = 1 to n do
    print_int i;
    print_char '\n';
    flush stdout;
    let arbres, arbrepos, _, _ = game_array.(i - 1) in
    assert (Array.length arbres = Array.length arbrepos);
    let carte, pos_arr = nv_generation taille_carte nb_villages in
    let evolue_arbre_tab, evolue_arbrepos_tab, arbre_scores =
      fait_genertion arbres arbrepos carte pos_arr
    in
    (* Stocke les arbres après évolution, là où ils ont évolués
       et les scores qu'on obtenu ces arbres *)
    game_array.(i) <-
      (evolue_arbre_tab, evolue_arbrepos_tab, pos_arr, arbre_scores)
    (* Stockage éventuel de la carte générée (pas indispensable) *)
    (* Yojson.to_file (Utils.ormat_carte_name i) ("dossier/" ^ serialize_carte carte) *)
  done;
  Yojson.to_file "game.json" (serialize_save_array game_array)
(*
   let game2 ?(nb_villages = 2) ?(nb_arbres = 20) ?(taille_carte = 200) (n : int) =
     let (game_array : save array) =
       Array.make (n + 1)
         ( Array.make nb_arbres Vide,
           Array.make nb_arbres Nil,
           Array.make nb_villages (-1, -1),
           Array.make_matrix nb_villages nb_arbres (-1) )
     in
     game_array.(0) <- (const (), [||], [||]);
     for i = 1 to n do
       let arbres, _, _ = game_array.(i - 1) in
       let carte, pos_arr = nv_generation taille_carte nb_villages in
       let evolue_arbre_tab, evolue_arbrepos_tab ,arbre_scores = fait_genertion arbres arbrepos carte pos_arr in
       game_array.(i) <- (evolue_arbre_tab, evolue_arbrepos_tab ,pos_arr, arbre_scores)
     done;
     Yojson.to_file "game.json" (serialize_save_array game_array) *)

let game i (n : int) =
  match i with 1 -> game1 n (* |2 -> game2 n  *) | _ -> ()
