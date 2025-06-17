open Village
open Mapmanage
open Mapgen
open Type
open Variable

exception Err01

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
  | MoinsBrut ->
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
  let troncon_tuiles = recup_troncon_tuiles troncon in
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
  let tab = recup_troncon_tuiles troncon in
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
  | [] -> raise Bloque
  | a :: _ ->
      let x, y = a in
      (* coin, largeur, hauteur *)
      let haut, gauche, droit, bas = (ref x, ref y, ref y, ref x) in
      let rec parc (pos_list : position list) =
        match pos_list with
        | [] ->
            let b, d, e, g = (!gauche, !droit, !haut, !bas) in
            ((e - 1, b - 1), d - b + 3, g - e + 3)
        | (a, b) :: q ->
            if a > !bas then bas := a;
            if a < !haut then haut := a;
            if b < !gauche then gauche := b;
            if b > !droit then droit := b;
            parc q
      in
      parc pos_list

let matrice_score_troncon pos_list carte pos_cardi =
  let coin, larg, haut = pos_cardi in
  let mat_score = Array.make_matrix haut larg 0 in
  let mat_bat_list = Array.make_matrix haut larg [] in
  let world_limit = Array.length carte in
  let limit mat world_limit coin larg haut =
    let x, y = coin in
    assert (x != world_limit && y != world_limit);
    for i = 0 to larg - 1 do
      for j = 0 to haut - 1 do
        if
          x + i < 0
          || x + i > world_limit - 1
          || y + j < 0
          || j + y > world_limit - 1
          || not (test_troncon_pas_plein carte.(x + i).(y + j))
        then mat.(i).(j) <- -10000
      done
    done
  in
  limit mat_score world_limit coin larg haut;
  (mat_score, mat_bat_list)

let remp_mat_bat_list matb mats pos_list carte pos_cardi =
  let world_limit = Array.length carte in
  let (x, y), larg, haut = pos_cardi in
  for i = 0 to larg - 1 do
    for j = 0 to haut - 1 do
      if mats.(i).(j) != -10000 && x + i < world_limit && y + j < world_limit
      then
        matb.(i).(j) <-
          sum_troncon_list
            (let (Troncon (a, _)) = carte.(x + i).(y + j) in
             a)
    done
  done

let calcul_mat_score mats matb arbrepos bat_origine =
  let rec trouve_bat bat list =
    match list with
    | [] -> 0
    | (a, b) :: q when a = bat -> b
    | _ :: q -> trouve_bat bat q
  in
  let rec trouve_cond bat list =
    match list with
    | [] -> (bat, taille_troncon * taille_troncon, false)
    | (a, b, c, d) :: q when a = bat -> (b, c, d)
    | e :: q -> trouve_cond bat q
  in
  let rec parcours_liane arbrepos mats matb bat_origine =
    if arbrepos = Nil then ()
    else
      let (Nodi (list, suite)) = arbrepos in
      let bat_cond, x, boo = trouve_cond bat_origine list in
      for i = 0 to Array.length mats - 1 do
        for j = 0 to Array.length mats.(0) - 1 do
          mats.(i).(j) <-
            (mats.(i).(j)
            +
            if trouve_bat bat_cond matb.(i).(j) > x then if boo then 1 else -1
            else 0)
        done
      done;
      parcours_liane suite mats matb bat_origine
  in
  parcours_liane arbrepos mats matb bat_origine

(* Parcours la matrice pour lister les positions les plus probables *)
let parc_mats_bat (arr : int array array) (coin : int * int) (carte : carte) =
  let a, b = coin in
  let c = ref (-9999) in
  let list = ref [] in
  for i = 0 to Array.length arr - 1 do
    for j = 0 to Array.length arr.(0) - 1 do
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
  let (Troncon (tronc, _)) = troncon in
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      if
        let (Tuile (a, _)) = tronc.(i).(j) in
        a = None
      then (
        modifie_batiment_dans_troncon carte carte.(x).(y) (Some bat) i j x y;
        raise Exit)
    done
  done;
  raise Not_found

let position_bat pos_list carte arbrepos bat_org =
  let pos_cardi = pos_card pos_list in
  let mats, matb = matrice_score_troncon pos_list carte pos_cardi in
  remp_mat_bat_list matb mats pos_list carte pos_cardi;
  calcul_mat_score mats matb arbrepos bat_org;
  let coin, _, _ = pos_cardi in
  let l = parc_mats_bat mats coin carte in
  let rec parc l =
    if l = [] then failwith "Pas de place"
    else
      let ((x, y) :: q) = l in
      try cons_bat carte x y carte.(x).(y) bat_org with
      | Not_found -> parc q
      | Exit -> ()
      | _ -> failwith "ici"
  in
  let tab = Array.of_list l in
  Array.shuffle ~rand:Random.int tab;
  let l1 = Array.to_list tab in
  parc l1

(* Effectue le type de construonction en fonction des paramètres *)
let a_faire (bat : batiment) (carte : carte) (village : village) : unit =
  if cout bat village then
    position_bat village.position_list carte village.arbrepos bat

(* Evalue un noeud et fait ce qu'il faut *)
let rec eval_noeud (node : arbre) (carte : carte) (village : village)
    (tester : bool ref) : unit =
  let ressource, _ = village.logistique in
  assert (not !tester);
  if not !tester then
    match node with
    | Vide -> failwith "Empty node"
    | Node (cond, sous_arbre_gauche, sous_arbre_droit, bat) ->
        let test_v = test ressource cond in
        if estVide sous_arbre_gauche && test_v then (
          a_faire bat carte village;
          tester := true)
        else if estVide sous_arbre_droit && not test_v then (
          a_faire bat carte village;
          tester := true)
        else if test_v then eval_noeud sous_arbre_gauche carte village tester
        else eval_noeud sous_arbre_droit carte village tester
  else raise Err01
