open Village
open Mapmanage
open Mapgen


(* Create the ratio of all ressources
let rec get_ratio (logistics : logistics) : data =
  match logistics with
  | [], _ :: _ | _ :: _, [] -> raise (Invalid_argument "Logistics tuple malformed (not the same length for both dicts)")
  | (e, _) :: _, (r, _) :: _ when e <> r -> raise (Invalid_argument "Logistics tuple malformed (Ressources do not match across dicts)")
  | [], [] -> []
  | (e, d) :: q, (_, f) :: s -> (e, d * 100 / f) :: get_ratio (q, s) *)


  let ingpercent r1 r2 ing x donnee:bool =
    let nr1 = search donnee r1 in
    let nr2 = search donnee r2 in
    match ing with
    |More -> begin 
      let dif = (nr1 - nr2)*100/nr1 in
      if nr1 > nr2 then (dif > x) else false
    end
    |Less -> begin 
      let dif = (nr1 - nr2)*100/nr1 in
      if nr1 < nr2 then (dif > x) else false
    end
  ;;
  let ingflat r1 r2 ing x donnee:bool =
    let nr1 = search donnee r1 in
    let nr2 = search donnee r2 in
    match ing with
    |More -> begin 
      let dif = nr1 - nr2 in
      if nr1 > nr2 then (dif > x) else false
    end
    |Less -> begin 
      let dif = nr1 - nr2 in
      if nr1 < nr2 then (dif > x) else false
    end
  ;;
let equalpercent r1 r2 x donnee =  
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in 
  let som = nr1 + nr2 in
  (dif*100/som < x)
;;
let equalflat r1 r2 x donnee = 
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in 
  let test = if dif < 0 then -dif else dif in 
  test < x
;;

  let test (donnee:data) (condition:condition):bool = 
  match condition with
  |Ingpercent   (r1,r2,ing,x) -> ingpercent r1 r2 ing x donnee
  |Ingflat      (r1,r2,ing,x) -> ingflat r1 r2 ing x donnee
  |Equalflat    (r1,r2,x)     -> equalflat r1 r2 x donnee
  |Equalpercent (r1,r2,x)     -> equalpercent r1 r2 x donnee


;;

(* Make all action in one turn *)
(* let evolution_par_tour (village : village) (map : map) =
   let _, tree, logistics, _, chunk_list = village in
   let stock_temp, _ = logistics in
   let logistics1 = (stock_temp, chunk_list_parcour chunk_list map) in
   let temp_logistics = update_all_logistics logistics1 in
   let new_logistics = lack_of_people temp_logistics logistics chunk_list map in

   let a, _ = new_logistics in

   (* let stockpile = match with *)

   (* Do action defined by the node, lack of the implementation of the village *)
   let to_do action = () in

   let rec eval tree =
     match tree with
     | Vide -> failwith "1.Invalid Argument"
     | Node (cond, tree_verif, tree_not_verif, act) -> (
         let is_condition_fulfilled = test ratio cond in
         match (tree_verif, tree_not_verif) with
         | Vide, Vide -> to_do act
         | Vide, a -> if is_condition_fulfilled then to_do act else eval a
         | a, Vide -> if is_condition_fulfilled then eval a else to_do act
         | a, b -> if is_condition_fulfilled then eval a else eval b)
   in
   eval tree
*)
