open Village
open Mapgen
open Newgen
open Score
open Dumpmap
open Mapmanage


(* A generation binds a map with the villages that live inside this map *)
type score = int array
type evaluation = score array 
type generation = tree array * map * position array * evaluation
type save =  tree array * position array * evaluation
type game = save array
;;

let new_game map_width nb_villages =
  let map = gen_map map_width in
  let roots = gen_village_roots (map_width / chunk_width) nb_villages in
  map ,roots
;;

(* let roots, trees = new_game 800 32;; *)

  (* Make all action in one turn *)
let evolution_par_tour (village : village) (map : map)  =
  let temp_logistics = update_all_logistics village.logistics in
   let new_logistics = lack_of_people temp_logistics village.logistics village.position_list map in
 ()
  (* EN COUR A FINIR *)
;;  

let init_logistique () = ([],[])

let starter_pack (map:map) (pos:position) =
  let x,y = pos in 
  mutate_building_in_chunk (map.(x).(y)) (Some Farm) 0 0 ;
  mutate_building_in_chunk (map.(x).(y)) (Some House) 0 1 


let createvillage (tree:tree) (pos:position) (map:map) (id:int) : village = (*init le village*)
 starter_pack map pos;{
  id = id;  
  tree = tree;
  logistics = init_logistique ();
  root_position = pos;
  position_list = [ pos ]
  }
;;

let nombre_de_tour_par_simulation = 10

let evalvilage a b = 
  for _ = 0 to nombre_de_tour_par_simulation do 
    evolution_par_tour a b 
  done;
  a

(* Fonctionne *)

let selection score tree_tab = (* selectionne les 20 meilleurs*)
  let quick_sort li =
    let comp x y = let _,a = x in let _,b = y in compare a b in 
    Array.sort comp li
  in
  let n = Array.length score in 
  let m = Array.length score.(0) in 
  let mat = Array.make_matrix n m (0,0) in 
  for i=0 to n-1 do 
    let li = Array.make m (0,0) in
    for j = 0 to m-1 do 
      li.(j) <- (j,score.(i).(j))
    done;
    quick_sort li;
    mat.(i) <- li
  done;
  for i=0 to n-1 do 
    for j = 0 to m-1 do
      let (x,_) = mat.(i).(j) in 
      mat.(i).(j) <- (x,m - j) 
    done
  done; 
  let rg = Array.make m [] in 
  for i=0 to n-1 do 
    for j = 0 to m-1 do 
      let (y,x) = mat.(i).(j) in 
      rg.(y) <- x :: rg.(y)
    done
  done; 
  let rec somme l = match l with
    |[] -> 0
    |e::q -> e + somme q 
  in
  let final = Array.make m (0,0) in
  for i=0 to m-1 do 
    let x = somme rg.(i) / n in 
    final.(i)<-(i,x)
  done;
  let comparer x y = 
    let xa,xb = x in 
    let ya,yb = y in 
    if xb = yb then compare xa ya else compare xb yb 
  in 
  let () = Array.sort comparer final in 
  let sortie = Array.make (m/5) Vide in 
  for i=0 to m/5 -1 do 
    let (x,_) = final.(i) in 
    sortie.(i) <- tree_tab.(x) 
  done;
  sortie
(* Fin de fonctionne *)
;;


(* fonctionne *)
let scoring (village:village) (map:map) :int =  (*la fct de score sur le village *) 
  calcul_score village map 
;;


let mutate (tree:tree array) :tree array = tree (* 'creer les 80 autres arbres *)
(* Eric *)
;;

let generaliser (a:save) (map:map) :generation  =
  let (h1,h2,h3) = a in 
  (h1 , map ,h2 ,h3)
;;

let do_genertion (generation:generation) :(tree array * evaluation)  =
  let tree_tab,map,pos_array,_ = generation in 
  let taille_pos = Array.length pos_array in 
  let taille_tree = Array.length tree_tab in
  let score = Array.make_matrix taille_tree taille_pos 0 in 
  for i = 0 to taille_pos-1 do 
    for j = 0 to taille_tree-1 do 
      let tempmap = Array.copy map in 
      let tempvilage = createvillage tree_tab.(j) pos_array.(i) tempmap j in
      let tempvilage = evalvilage tempvilage tempmap in
      let scoretour = scoring tempvilage tempmap in
      score.(i).(j) <- scoretour ;
    done
  done;
  let new_tree = selection score tree_tab in
  let new_tree = mutate new_tree in
  (new_tree,score)
;;

let game ?(nb_villages = 32) ?(nb_trees = 100) ?(taille_map = 800 ) (n:int) = 
  let (tab:game) = Array.make n ( (Array.make nb_trees Vide), (Array.make nb_villages (-1,-1)), (Array.make_matrix nb_villages nb_trees (-1))) in
  (* Init de la première gen d'arbre *)
  let tree_tab1 = gen_trees nb_trees in 
  (* Map gen *)
  let (map,pos_list) = new_game taille_map nb_villages in
  tab.(0) <- (tree_tab1,pos_list,[||]);
  for i = 1 to n-1 do 
    let tree_tab,score = (do_genertion (generaliser (tab.(i-1)) map )) in 
    let h1,h2,_= tab.(i-1) in 
    tab.(i-1) <- (h1,h2,score) ;
    (* Map gen *)
    let map,pos_list = new_game taille_map nb_villages in
    Yojson.to_file "efopzvipbaqspivbvqsopvh" (serialize_map map);
    tab.(i) <- (tree_tab,pos_list,[||])
  done;
  Yojson.to_file "game.json" (serialize_save_array tab)
;;
  
