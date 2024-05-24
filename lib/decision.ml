
(*open Village*)
(*              Temp *)
type biome = Forest | Desert | Plains
type building = House | Quarry | Sawmill | Farm 
type tile = (building option) * int
type chunk = ((tile array) array) * biome * int
let chunk_width =4 ;; 
type map = ((chunk array) array)
type ressource = Food | People | Stone | Wood | Bed 
type data = ((ressource * int) list)
type logistics = data * data
type position = int * int
type ing = Surplus | Lack 
type verb = Build 
type action = verb * building
type condition = int * ing * ressource
type tree = Vide | Node of condition * tree * tree * action  
type village = int * tree * logistics * position *  position list 



(* Create the new logistics *)
let rec update_logistics logistics = match logistics with 
  | [], _::_ | _::_, [] -> failwith "2.Lack ressource"
  | (e, _)::_, (r, _)::_ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> [] ,[]
  | (e, d)::q, (_, f)::s -> let (new_stock, need) = ((e, (d + f) ) , (e,f)) in  
                            let (a,b)= update_logistics (q, s) in
                            (new_stock :: a , need::b)
;;

(* Create the ratio of all ressources *)

let rec get_ratio logistics = match logistics with 
  | [], _::_ | _::_, [] -> failwith "2.Lack ressource"
  | (e, _)::_, (r, _)::_ when e <> r -> failwith "3.Not the same ressource"
  | [], [] -> []
  | (e, d)::q, (_, f)::s -> (e, d*100/f )::(get_ratio (q, s))


(* Evaluates to the amount of the passed ressource that is con/cal *)
let rec search (data:data) ressource = match data with 
  | [] -> failwith "4.Not Defined"
  | (e, x)::_ when e = ressource -> x 
  | _::q -> search q ressource
;; 

(* Tests if the passed condition is fullfilled *)
let test difference condition = 
  let needed_percent, inequality, ressource = condition in
  (* Retrieves the ratio between the stocks and the needs *)
  let ressource_ratio = search difference ressource in
  let ratio = ressource_ratio - needed_percent in
  match ratio, inequality with
    | x, Lack when x > 0 -> false 
    | x, Surplus when x < 0 -> false
    | x, _ -> (x > needed_percent)
;;
(* Make all action in one turn *)
let evolution_par_tour village = 
  let (_, tree, logistics, _) = village in
  let new_logistics = update_logistics logistics in 
  let ratio = get_ratio new_logistics in
  let a, _ = new_logistics in
  let stockpile = match 


  (* Do action defined by the node, lack of the implementation of the village *)
  
  
  let to_do action = () in
  
  
  let rec eval tree = match tree with 
    | Vide -> failwith "1.Invalid Argument"
    | Node (cond, tree_verif, tree_not_verif, act) ->
      let is_condition_fulfilled = test ratio cond in
      match (tree_verif, tree_not_verif) with 
        | Vide, Vide -> to_do act 
        | Vide, a -> 
            if is_condition_fulfilled then to_do act else eval a
        | a, Vide -> 
            if is_condition_fulfilled then eval a else to_do act
        | a, b -> 
            if is_condition_fulfilled then eval a else eval b 
  in
  eval tree
;;

let stock_exp:data = [(Bed,0);(Food,0);(People,0);(Stone,0);(Wood,0)]

let needed_exp:data = [(Bed,0);  (Food,100);(People,-25); (Stone,0);  (Wood,0)]

let village_exp:village = 1, Vide, (stock_exp, needed_exp), (0, 0), [] 
