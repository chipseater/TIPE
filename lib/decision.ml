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

let ingpercent r1 r2 ing x donnee : bool =
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

let test_not_full (chunk : chunk) : bool =
  match chunk with
  | None -> failwith "izeovoap"
  | Chunk (x, _) ->
      let t = ref false in
      for i = 0 to chunk_width do
        for j = 0 to chunk_width do
          let (Tile (c, _)) = x.(i).(j) in
          if c == None then t := true
        done
      done;
      !t

let possibilite chunk =
  let arr = Array.make 16 (-1, -1) in
  match chunk with
  | None -> failwith "izviuaebiazvbaui"
  | Chunk (tab, _) ->
      (let c = ref 0 in
       for i = 0 to chunk_width do
         for j = 0 to chunk_width do
           let (Tile (b, _)) = tab.(i).(j) in
           if b == None then arr.(!c) <- (i, j);
           c := !c + 1
         done
       done);
      arr

let buildtile (build : building) (map : map) (table : (int * int) array) =
  shuffle table;
  let x, y = table.(0) in
  let temp = map.(x).(y) in
  let arr = possibilite temp in
  shuffle arr;
  let rec choice arr c =
    match arr.(c) with
    | -1, -1 -> choice arr (c + 1)
    | i, j -> mutate_building_in_chunk map.(x).(y) (Some build) i j
  in
  choice arr 0

let pos_card (pos_list : position list) =
  match pos_list with
  | [] -> failwith "nbevoibqaogv"
  | a :: _ ->
      let x, y = a in
      let top, left, right, bot = (ref x, ref y, ref y, ref x) in
      (* corner, largeur, hauteur *)
      let rec parc (pos_list : position list) =
        match pos_list with
        | [] ->
            let b, d, e, g = (!left, !right, !top, !bot) in
            ((e + 1, b + 1), d - b, g - e)
        | (a, b) :: q ->
            if a > !bot then bot := a;
            if a < !top then top := a;
            if b < !left then left := b;
            if b > !right then right := b;
            parc q
      in
      parc pos_list

let rec proxi (arr : int array array) (pos_list : position list) (corner : position)
    =
  let p, m = corner in
  match pos_list with
  | [] -> ()
  | (x, y) :: q ->
      arr.(x - 1 - p).(y - 1 - m) <- arr.(x - 1 - p).(y - 1 - m) + 1;
      arr.(x - 1 - p).(y - m) <- arr.(x - 1 - p).(y - m) + 1;
      arr.(x - 1 - p).(y + 1 - m) <- arr.(x - 1 - p).(y + 1 - m) + 1;
      arr.(x - p).(y - 1 - m) <- arr.(x - p).(y - 1 - m) + 1;
      arr.(x - p).(y - m) <- -10;
      arr.(x - p).(y + 1 - m) <- arr.(x - p).(y + 1 - m) + 1;
      arr.(x + 1 - p).(y - 1 - m) <- arr.(x + 1 - p).(y - 1 - m) + 1;
      arr.(x + 1 - p).(y - m) <- arr.(x + 1 - p).(y - m) + 1;
      arr.(x + 1 - p).(y + 1 - m) <- arr.(x + 1 - p).(y + 1 - m) + 1 ; proxi arr q corner



let parc_mat (arr : int array array) (h : int) (l : int) =
  let c = ref 0 in
  let list = ref [] in
  for i = 0 to h do
    for j = 0 to l do
      if arr.(i).(j) > !c then list := [ (i, j) ]
      else if arr.(i).(j) = !c then list := (i, j) :: !list
      else ()
    done
  done;
  !list

let buildout (build : building) (map : map) (pos_list : position list) : unit =
  let coner, larg, haut = pos_card pos_list in
  let mat = Array.make_matrix (haut + 2) (larg + 2) 0 in
  proxi mat pos_list coner;
  let list = parc_mat mat haut larg in
  let arr = Array.of_list list in
  buildtile build map arr

let buildin (build : building) (map : map) (pos_list : position list) =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_not_full map.(x).(y) == false -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  match temp with
  | [] -> buildout build map pos_list
  | _ :: _ ->
      let tab = Array.of_list temp in
      buildtile build map tab

let to_do (action : action) (map : map) (pos_list : position list) : unit =
  let arg, build, pref = action in
  if pref = Random then
    match arg with
    | InCity -> buildin build map pos_list
    | OutCity -> buildout build map pos_list
  else ()

let rec eval_node (node : tree) (ressource : data) (pos_list : position list)
    (map : map) =
  match node with
  | Vide -> failwith "Empty node"
  | Node (cond, sub_tree_left, sub_tree_right, action) -> (
      match test ressource cond with
      | true -> (
          match sub_tree_left with
          | Vide -> to_do action map pos_list
          | Node (a, b, c, d) ->
              eval_node (Node (a, b, c, d)) ressource pos_list map)
      | false -> (
          match sub_tree_right with
          | Vide -> to_do action map pos_list
          | Node (a, b, c, d) ->
              eval_node (Node (a, b, c, d)) ressource pos_list map))
