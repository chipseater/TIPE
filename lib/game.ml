open Village
open Mapgen
open Newgen

(* A generation binds a map with the villages that live inside this map *)
type score = int array
type evaluation = (int * score) list
type generation = tree array * map * positions array * evaluation
type game = generation array




let new_game map_width nb_village =
  let map = gen_map map_width in
  let villages = new_villages map_width nb_village in
  [ (villages, map) ]

;;

  (* Make all action in one turn *)
 let evolution_par_tour (village : village) (map : map) =
   let id, tree, logistics, _, chunk_list = village in
   let temp_logistics = update_all_logistics logistics in
   let new_logistics = lack_of_people temp_logistics logistics chunk_list map in

 ;;
 let creatvillage (tree:tree) (pos:position) (map:map) : village = () (*init le village*)
;;
let selection (score:evaluation) : tree array = () (* selectionne les 20 meilleurs*)
;;
let scoring (village:village) :int = () (*la fct de score sur le village *) 
;;
let mutate (tree:tree array):tree array = () (* 'creer les 80 autres arbres *)
;;
 let do_genertion (generation:generation) :(tree array * evaluation)  =
  let tree_tab,map,pos_array,_ = generation in 
  let taille_pos = Array.length pos_array in 
  let taille_tree = Array.length tree_tab in
  let score = Array.makematrix 0 taille_tree taille_pos in 
  for i = 0 to taille_pos-1 do 
    for j = 0 to taille_tree-1 do 
      let tempmap = Array.copy map in 
      let tempvilage = creatvillage tree_tab.(j) pos_array.(i) tempmap
      evalvilage tempvilage tempmap;
      let scoretour = scoring village in
      score.(i).(j) = scoretour;
    done
  done
  let new_tree = selection score in
  let new_tree = mutate new_tree in
  (new_tree,score)
  ;;

  let game (n:int) = 
    let tab = Array.make ([| |],[| |],[| |],[]) n in
    (* Init de la première gen d'arbre *)
    let tree_tab1 = () in 
    (* Map gen *)
    let (map,pos_list) = () in
    tab.(0) = (tree_tab1,map,pos_list,[||]) ;
    for i = 1 to n-1 do 
      let tree_tab,score = (do_genertion tab.(i-1) ) in 
      let h1,h2,h3,_ = tab.(i-1) in 
      tab.(i-1) = (h1,h2,h3,score) ;
      (* Map gen *)
      let (map,pos_list) = () in
      tab.(i) = (tree_tab,map,pos_list,[||]) ;
    done
  let () = Yojson.to_file "game.json" (serialize_game tab)
  

      





