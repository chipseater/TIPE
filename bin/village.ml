(* Village *)
type ressource = Food | People | Stone | Wood | Bed 
(* Contains a dictionnary with the ressources status *)
type data = ((ressource * int) list)
(* Contains both the needed ressources and the village's stockpiles *)
type logistics = data * data
type id = int

(* Tree *)
type ing = Surplus | Lack 
type verb = Build | Overwrite
type action = verb * building
type condition = int * ing * ressource
type tree = Vide | Node of condition * tree * tree * action  
type building = House | Quarry | Sawmill | Farm 

type village = id * tree * logistics * (int * int)  

(* Fonction *)
let rec calcul (stock:data) (needed:data) = match (stock,needed) with 
  |[], e::q |e::q ,[] -> failwith("2.Lack ressource") 
  |(e,d)::q ,(r,f)::s when e != r -> failwith("3.Not the same ressource")
  |[],[]              -> []
  |(e,d)::q ,(r,f)::s -> (e,(d-f)) :: (calcul q s)
;;
let rec search (data:data) ressource = match data with 
  |[] -> failwith("4.Not Defined")
  |(e,x)::q when e = ressource -> x 
  |e::q -> search q ressource
;;
let evolution village = 
  let (id,tree,(old_stock, needed), pos) =village in 
  let tab = calcul old_stock needed in
  let test (p1,p2,p3) = (*a pourcent (Exp : 20% or more) / Lack or Surplus / ressource *)
    let need = search needed p3 in
    let ressource_stock = search tab p3 in 
    let ratio = (ressource_stock - need)*100/need in 
    match ratio,p2 with  
      |x, Lack when x > 0 -> false 
      |x, Surplus when x < 0 -> false
      |x, _ -> (x > p1)
  in
  let to_do (v,b) =
    ()
    (* Do action defined by the node, lack of the implementation of the village *)
  in
  let rec eval tree = match tree with 
    |Vide -> failwith("1.Invalid Argument") (*Invalid Arg*)
    |Node(cond,tree_verif, tree_not_verif,act) -> begin 
      match (tree_verif, tree_not_verif) with 
        |Vide,Vide -> to_do act 
        |Vide,a -> if (test cond) then (to_do act)  else (eval a)  
        |a,Vide -> if (test cond) then (eval a)     else (to_do act)
        |a,b    -> if (test cond) then eval a       else eval b 
    end
  in
  eval tree
;;

