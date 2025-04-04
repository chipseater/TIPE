open Village
open Mapmanage
open Mapgen
open Type


exception Couille

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
        let ratio = nb_ressource2 * 100 / nb_ressource1 in
        if nb_ressource1 > nb_ressource2 then ratio > pourcentage else false
  | LessPercent ->
      if nb_ressource1 = 0 then false
      else
        let ratio = nb_ressource2 * 100 / nb_ressource1 in
        if nb_ressource1 < nb_ressource2 then ratio > pourcentage else false

(* Test si la ressource n1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le minimum est inférieur à la diférence *)
let inegalitebrut ressource1 ressource2 ing min donnee : bool =
  let nb_ressource1 = recherche donnee ressource1 in
  let nb_ressource2 = recherche donnee ressource2 in
  match ing with
  | PlusBrut ->
      let dif = nb_ressource1 - nb_ressource2 in
      if nb_ressource1 > nb_ressource2 then dif > min else false
  | MoinBrut ->
      let dif = nb_ressource1 - nb_ressource2 in
      if nb_ressource1 < nb_ressource2 then -dif > min else false
  | EquivalentBrut -> abs (nb_ressource2 - nb_ressource1) < min

(* Effectue le test selon l'objet *)
let test (donnee : donne) (condition : condition) : bool =
  match condition with
  | InegaliteEnPourcentage (ressource1, ressource2, ing, pourcentage) ->
      inegaliteenpourcentage ressource1 ressource2 ing pourcentage donnee
  | InegaliteBrute (ressource1, ressource2, ing, min) ->
      inegalitebrut ressource1 ressource2 ing min donnee

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


let matrice_score_troncon pos_list carte pos_cardi =
  let corner, larg, haut = pos_cardi in
  let mat_score = Array.make_matrix haut larg 0 in
  let mat_bat_list = Array.make_matrix haut larg [] in
  let world_limit = Array.length carte in
  let limit mat world_limit corner larg haut = 
    let (x,y) = corner in 
    for i=0 to larg do 
      for j=0 to haut do 
         if x+i < 0 || x+i > world_limit -1 || y+j < 0 || j+y > world_limit -1 || (not (test_troncon_pas_plein carte.(x).(y))) then mat.(i).(j) <- -10000 
        done
      done
  in limit mat_score world_limit corner larg haut ;
  (mat_score,mat_bat_list)

let remp_mat_bat_list matb mats pos_list carte pos_cardi =
  let (x,y), larg, haut = pos_cardi in
  for i=0 to larg do 
    for j = 0 to haut do 
      if mats.(x+i).(y+j) < 0 then 
        matb.(x+i).(y+j) <- sum_troncon_list (let Troncon(a,_) = carte.(x+i).(y+j) in a)
    done
  done
  
let calcul_mat_score mats matb treepos bat_origine =
  let rec trouve_bat bat list = match list with
    |[] -> 0
    |(a,b)::q when a = bat -> b
    |_::q -> trouve_bat bat q 
  in
  let rec trouve_cond bat list = match list with 
    |[] -> (bat,-1,false)
    |(a,b,c,d)::q when a = bat -> (b,c,d)
    |e::q -> trouve_cond bat q
  in
  let rec parcours_liane treepos mats matb bat_origine =
    if treepos = Nil then () 
    else let Nodi(list,suite) = treepos in   
    let bat_cond,x,boo = trouve_cond bat_origine list in 
    for i =0 to Array.length mats do 
      for j=0 to Array.length mats.(0) do 
        mats.(i).(j) <- mats.(i).(j) + if trouve_bat bat_cond (matb.(i).(j)) > x then (if boo then 1 else -1) else 0
      done
    done;
    parcours_liane suite mats matb bat_origine
  in parcours_liane treepos mats matb bat_origine


(* Parcours la matrice pour lister les positions les plus probables *)
let parc_mats_bat (arr : int array array) (corner : int * int)
    (carte : carte) =
  let a, b = corner in
  let c = ref 1 in
  let list = ref [] in
  for i = 0 to (Array.length arr) - 1 do
    for j = 0 to (Array.length arr.(0)) - 1 do
      if arr.(i).(j) > !c && test_troncon_pas_plein carte.(i + a).(j + b) then (
        list := [ (i + a, j + b) ];
        c := arr.(i).(j))
      else if arr.(i).(j) = !c && test_troncon_pas_plein carte.(i + a).(j + b)
      then list := (i + a, j + b) :: !list
      else ()
    done
  done;
  !list

let cons_bat carte x y troncon bat =
  let Troncon(tronc,_) = troncon in   
  for i=0 to taille_troncon - 1 do 
    for j=0 to taille_troncon -1 do
      if let Tuile(a,_) = (tronc.(i).(j)) in a = None 
        then (modifie_batiment_dans_troncon carte carte.(x).(y) (Some bat) i j; 
      raise Exit)
    done
  done;
  raise Not_found

let position_bat pos_list carte treepos bat_org = 
  let pos_cardi = pos_card pos_list in 
  let (mats,matb) = matrice_score_troncon pos_list carte pos_cardi in 
  remp_mat_bat_list matb mats pos_list carte pos_cardi;
  calcul_mat_score mats matb treepos bat_org;
  let (corner,_,_)=pos_cardi in 
  let l = parc_mats_bat mats corner carte in 
  let rec parc l = 
    if l = [] then failwith "Pas de place"
    else
    let (x,y) ::q = l in 
    try 
      cons_bat carte x y carte.(x).(y) bat_org
    with 
    |Not_found -> parc q 
    |Exit -> ()
  in
  let tab = Array.of_list l in 
  Array.shuffle ~rand:Random.int tab;
  let l1 = Array.to_list tab in 
  parc l1

(* Effectue le type de construonction en fonction des paramètres *)
let a_faire (bat:batiment) (carte : carte) (village : village) : unit =
  position_bat village.position_list carte village.treepos bat 


(* Evalue un noeud et fait ce qu'il faut *)
let rec eval_node (node : tree) (carte : carte) (village : village) (tester: bool ref) : unit =
  let ressource, _ = village.logistique in
  assert (not !tester);
  if not !tester then
    match node with
    | Vide -> failwith "Empty node"
    | Node (cond, sub_tree_left, sub_tree_right, bat) ->
        let test_v = test ressource cond in
        if estVide sub_tree_left && test_v then 
          (a_faire bat carte village; tester := true)
        else if estVide sub_tree_right && not test_v then
          (a_faire bat carte village; tester := true)
        else if test_v then eval_node sub_tree_left carte village tester
        else eval_node sub_tree_right carte village tester
  else
    raise Couille
