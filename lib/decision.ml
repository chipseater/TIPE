open Village
open Mapmanage
open Mapgen

let shuffle arr =
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done
;;
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
  match ing with
  | More ->
      let dif = (nr1 - nr2) * 100 / nr1 in
      if nr1 > nr2 then dif > x else false
  | Less ->
      let dif = (nr1 - nr2) * 100 / nr1 in
      if nr1 < nr2 then dif > x else false

let ingflat r1 r2 ing x donnee : bool =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  match ing with
  | More ->
      let dif = nr1 - nr2 in
      if nr1 > nr2 then dif > x else false
  | Less ->
      let dif = nr1 - nr2 in
      if nr1 < nr2 then dif > x else false

let equalpercent r1 r2 x donnee =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in
  let som = nr1 + nr2 in
  dif * 100 / som < x

let equalflat r1 r2 x donnee =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  let dif = nr1 - nr2 in
  let test = if dif < 0 then -dif else dif in
  test < x

let test (donnee : data) (condition : condition) : bool =
  match condition with
  | Ingpercent (r1, r2, ing, x) -> ingpercent r1 r2 ing x donnee
  | Ingflat (r1, r2, ing, x) -> ingflat r1 r2 ing x donnee
  | Equalflat (r1, r2, x) -> equalflat r1 r2 x donnee
  | Equalpercent (r1, r2, x) -> equalpercent r1 r2 x donnee
;;

let test_not_full (chunk:chunk) : bool =
    match chunk with
      | None -> failwith("izeovoap")
      | Chunk(x,_) -> 
  begin
    let t = ref false in
    for i= 0 to chunk_width do 
      for j = 0 to chunk_width do 
        let Tile(c,_) = x.(i).(j) in 
        if c == None then t := true
      done 
    done; !t
  end

;;
let buildtile (build:building) (map:map) (table:(int*int) array)= 
shuffle table ;

()

;;
let buildin (build:building) (map:map) (pos_list:position list) =
  
(*
let buildin  (build:building) (map:map) (pos_list:position list) = 
  let n =   List.length pos_list in
  if n < 230 then begin
  let table = Array.make (n) (0,0) in 
    let rec parc pos_list compt = match pos_list with
    | e::q -> table.(compt) <- e; parc q (compt+1)
    |[] -> ()
in parc pos_list 0
; buildtile build map table
end
else begin 
  let x = (n / 230)+1 in
let table = Array.make_matrix x (n) (0,0) in 
  let rec parc pos_list comptcolone compt = match pos_list,compt with
  | _, c when c = 230 -> parc pos_list (comptcolone+1) 0
  | e::q,_ -> table.(comptcolone).(compt) <- e; parc q comptcolone (compt+1)
  |[],_ -> ()
in parc pos_list 0 0 
;
let () = Random.self_init () in
buildtile build map table.(Random.int x)
end
;;
*)
let buildout (build:building) (map:map) (pos_list:position list) = 

;;

let to_do (action:action) (map:map) (pos_list:position list) : unit = 
  let arg,build,pref = action in 
  if pref = Random then
  match arg with
  | InCity  -> buildin build map pos_list
  | OutCity -> buildout build map pos_list
  else 
;;

let rec eval_node (node:tree) (ressource:data) (pos_list:position list) (map:map) = match node with
| Vide -> failwith ("Empty node")
| Node(cond,sub_tree_left, sub_tree_right,action) -> match (test ressource cond) with
| true -> (match sub_tree_left with
          | Vide -> to_do action map pos_list
          | Node(a,b,c,d) -> eval_node (Node(a,b,c,d)) ressource
  )
|false -> (match sub_tree_right with
          | Vide -> to_do action map pos_list
          | Node(a,b,c,d) -> eval_node (Node(a,b,c,d)) ressource
)






