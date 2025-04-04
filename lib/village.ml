open Mapmanage
open Type
(* Fonction *)
(* Additionne deux dictionnaires de ressources *)
let rec sum_donne (l1 : donne) (l2 : donne) : donne =
  match (l1, l2) with
  | (r1, _) :: _, (r2, _) :: _ when r1 <> r2 ->
      raise (Invalid_argument "Not the same ressource's place")
  | [], [] -> []
  | _, [] | [], _ -> raise (Invalid_argument "Not the same size")
  | (r1, v1) :: q1, (_, v2) :: q2 -> (r1, v1 + v2) :: sum_donne q1 q2


(* Renvoie la production de la tuile d'après le batiment qu'il contient *)
let get_production_from bat : donne =
  match bat with
  | Maison -> maison_donne_prodution
  | Carriere -> carriere_donne_prodution
  | Ferme -> ferme_donne_prodution
  | Scierie -> scierie_donne_prodution
  | _ -> void_donne


(* Renvoie la production de la tuile d'après le batiment qu'il contient *)
let get_production_from_tuile (tuile : tuile) : donne =
  match get_tuile_batiment tuile with
  | Some Maison -> maison_donne_prodution
  | Some Carriere -> carriere_donne_prodution
  | Some Ferme -> ferme_donne_prodution
  | Some Scierie -> scierie_donne_prodution
  | None -> void_donne


let rec mult_donne l n = 
  match l with 
  |[] -> []
  |(a,b)::q -> (a,n*b) :: mult_donne q n 
let rec mult_donne_ress l n r = 
  match l with 
  |[]-> []
  |(a,b)::q when r = a -> (a,n*b) :: mult_donne_ress q n r
  |(a,b)::q -> (a,b) :: mult_donne_ress q n r


(* Somme la prodution dans un troncon *)
let sum_troncon_list troncon =
  let rec parc l bat = 
    match l with
    |[] -> (bat,1) :: []
    |(a,b)::q when a = bat -> (a,(b+1)) :: q 
    |e :: q -> e:: parc q bat
  in
  let rec sum_troncon troncon i j l = 
    if j+1 = taille_troncon then ( 
      if i+1 = taille_troncon then (
        if let Tuile( a,_) =  troncon.(i).(j) in a != None then 
        let bat = let Tuile( Some e,_)=  troncon.(i).(j) in e in 
        parc l bat else l ) 
      else(if let Tuile(a,_) =  troncon.(i).(j) in a != None then
        let bat = let Tuile( Some e,_)=  troncon.(i).(j) in e in 
        sum_troncon troncon (i+1) 0 (parc l bat) else sum_troncon troncon (i+1) 0 l )
      )else
        (if let Tuile( a,_) =  troncon.(i).(j) in a != None then
      let bat = let Tuile( Some e,_)=  troncon.(i).(j) in e in 
      sum_troncon troncon i (j+1) (parc l bat) else sum_troncon troncon i (j+1) l)
    in 
    sum_troncon troncon 0 0 []

let rec taill l = match l with 
  |[] -> 0
  |_:: q -> 1 + taill q

let ferme_modif b = 
  let n = float_of_int b in 
  [|1.;sqrt n;1.;1.;1.|]
  
let puit_modif b = 
  match b with
  |0 -> 1.
  |1 -> 1.75
  |2 -> 2.5
  |3 -> 1.2
  |4 -> 0.5
  |_ -> 0.



let modif affec orig nb = match affec,orig with 
  |Ferme,Ferme -> ferme_modif nb
  |_ -> void_modif


let test_mod bat tab = let a,b = tab in 
  modif bat a b 


let produi tab2 tab1 = 
  let tab = Array.make (Array.length tab2) 1. in 
  for i=0 to (Array.length tab)-1 do
    tab.(i) <- tab2.(i) *. tab1.(i)
  done;
  tab


(* une liste qui contiennent les bat et leur nb *)
let tabl_mod prod_list = 
  let rec construction bat list = match list with
    |[] -> void_modif 
    |a::q -> produi (test_mod bat a) (construction bat q)
  in
  let (a,_)::_ = prod_list in 
  let i = taill prod_list in 
  let mat = Array.make i (a,void_modif) in
  let rec aff l j = 
    if j = i then ()
    else let (a,_)::q = l in 
      let n = mat.(j) in 
      mat.(j) <- (a,construction a prod_list);
      aff q (j+1)
  in
  aff prod_list 0 ; mat

let produit_d_f don flo = 
  let tab = Array.of_list don in 
  for i = 0 to (Array.length tab)-1 do
    let (o,p) = tab.(i) in 
    tab.(i) <- o,int_of_float ((float_of_int (p )*. flo.(i)) )
  done;
  Array.to_list tab
let get_modif tab a =
    let rec parc i = 
      let n = Array.length tab in 
      if i =n then void_modif else 
        if let (e,_) = tab.(i) in e = a then let (_,b) = tab.(i) in b 
        else parc (i+1) 
    in parc 0 
 
let sum_troncon_production troncon = 
  let Troncon(tab,_) = troncon in 
  let lis = sum_troncon_list tab in
  let tab = tabl_mod lis in
  let rec recup lis = match lis with 
    |(a,b)::q -> sum_donne (produit_d_f (mult_donne (get_production_from a) b) (get_modif tab a))  (recup q)
    |[] -> void_donne
  in 
  recup lis



let rec somme_troncon_list_production (troncon_list : position list)
    (carte : carte) =
  match troncon_list with
  | (i, j) :: q ->
      let production = sum_troncon_production carte.(i).(j) in
      sum_donne production (somme_troncon_list_production q carte)
  | [] -> void_donne

(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec recherche (donne : donne) ressource =
  match donne with
  | [] -> raise (Invalid_argument "Ressource not found in donne dict")
  | (e, x) :: _ when e = ressource -> x
  | _ :: q -> recherche q ressource




(* Inititalisation d'un objet logistique *)
let rec update_logistique (logistique : logistique) : logistique =
  match logistique with
  | [], _ :: _ | _ :: _, [] -> failwith "2.Lack ressource"
  | (e, _) :: _, (r, _) :: _ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> ([], [])
  | (e, d) :: q, (_, f) :: s ->
      let nouvel_stock, prod = ((e, d + f), (e, 0)) in
      let a, b = update_logistique (q, s) in
      (nouvel_stock :: a, prod :: b)

let calcul_of_main_d_oeuvre donne =
  let nouriture = recherche donne Nouriture in
  let bed = recherche donne Bed in
  let main_d_oeuvre = recherche donne Main_d_oeuvre in
  if main_d_oeuvre > bed * 10 then
    if bed > nouriture then
      sum_donne donne
        [
          (Bed, -bed);
          (Nouriture, -nouriture);
          (Main_d_oeuvre, -main_d_oeuvre + (nouriture * 10));
          (Pierre, 0);
          (Wood, 0);
        ]
    else
      sum_donne donne
        [
          (Bed, -bed);
          (Nouriture, -bed);
          (Main_d_oeuvre, -main_d_oeuvre + (bed * 10));
          (Pierre, 0);
          (Wood, 0);
        ]
  else if main_d_oeuvre > nouriture * 10 then
    sum_donne donne
      [
        (Bed, -bed);
        (Nouriture, -nouriture);
        (Main_d_oeuvre, -main_d_oeuvre + (nouriture * 10));
        (Pierre, 0);
        (Wood, 0);
      ]
  else
    let remaining_nouriture = nouriture - (main_d_oeuvre / 10) in
    let remaining_beds = bed - (main_d_oeuvre / 10) in
    let last_gen_main_d_oeuvre = main_d_oeuvre / 10 * 10 in
    if remaining_beds * 2 > remaining_nouriture then
      sum_donne donne
        [
          (Bed, -bed);
          (Nouriture, -nouriture + (remaining_nouriture mod 2));
          ( Main_d_oeuvre,
            -main_d_oeuvre + last_gen_main_d_oeuvre
            + (remaining_nouriture / 2 * 10) );
          (Pierre, 0);
          (Wood, 0);
        ]
    else
      sum_donne donne
        [
          (Bed, -bed);
          (Nouriture, -nouriture + remaining_nouriture - (2 * remaining_beds));
          ( Main_d_oeuvre,
            -main_d_oeuvre + last_gen_main_d_oeuvre + (2 * remaining_beds * 10)
          );
          (Pierre, 0);
          (Wood, 0);
        ]

let update_main_d_oeuvre (logistique : logistique) : logistique =
  match logistique with
  | stock, prod -> ((calcul_of_main_d_oeuvre stock : donne), prod)

(* Calcul la nouvelle table de donne *)
let update_all_logistique (logistique : logistique) position_list carte =
  let a, _ = logistique in
  let b = somme_troncon_list_production position_list carte in
  let nouvel_logistique = update_logistique (a, b) in
  (nouvel_logistique : logistique)

(* Calcule la nouvelle table de donnée en modifiant la carte *)
(* Calcule la logistique à chaque tuile et a chaque fois que la
   resource main d'oeuvre devient négative je change la case en none
   et je recalcule la nouvelle table
*)


let destroy_batiment (logistique : logistique) (position_list : position list)
    (carte : carte) : logistique =
  let stoc, _ = logistique in
  let parcours_troncon (troncon : troncon) (stock : donne) =
    let main_d_oeuvre = ref (recherche stock Main_d_oeuvre) in
    let temp_stock = ref stock in
    for i = 0 to taille_troncon - 1 do
      for j = 0 to taille_troncon - 1 do
        let tuile_donne =
          get_production_from_tuile (get_troncon_tuiles troncon).(i).(j)
        in
        let main_d_oeuvre_need = recherche tuile_donne Main_d_oeuvre in
        if !main_d_oeuvre > -main_d_oeuvre_need then (
          main_d_oeuvre := !main_d_oeuvre - main_d_oeuvre_need;
          temp_stock := sum_donne !temp_stock tuile_donne)
        else modifie_batiment_dans_troncon carte troncon None i j
      done
    done;
    !temp_stock
  in
  let rec parcours_list (l : position list) (stock : donne) =
    match l with
    | [] -> failwith "Invalid Arg d.1"
    | (x, y) :: [] -> parcours_troncon carte.(x).(y) (stock : donne)
    | (x, y) :: q ->
        parcours_list q (parcours_troncon carte.(x).(y) (stock : donne))
  in
  let nouvel_prod = parcours_list position_list stoc in
  update_all_logistique (stoc, nouvel_prod) position_list carte

let lack_of_main_d_oeuvre (logistique : logistique)
    (old_logistique : logistique) (troncon_list : position list) (carte : carte)
    =
  let donne, _ = logistique in
  if recherche donne Main_d_oeuvre < 0 then
    destroy_batiment old_logistique troncon_list carte
  else logistique
