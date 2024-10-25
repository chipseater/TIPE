open Game
let _ = game 2



(* let aff arr = 
  let n = Array.length arr in
  let m = Array.length arr.(1) in
  for i = 0 to (n-1) do 
    for j =0 to (m-1) do 
      print_int (arr.(i).(j));print_char ' ';print_char ' ';print_char ' ';print_char ' ';print_char ' ';print_char ' ';print_char ' ';print_char ' ' 
    done ;
    print_char '\n'
  done
let rec aff2 tab = 
  match tab with 
  |e::q -> let a,b = e in print_int a;print_char ' ' ; print_int b ;print_char '\n';  aff2 q
  |[]->()
  
;;
let mock_map =
  ([|[|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |]|],  Desert );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  |]; 
  [|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|    [| Tile (None, 10); Tile (None, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];    [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];    [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];    [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];  |],  Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|    [| Tile (None, 10); Tile (None, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];    [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];    [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];    [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];  |],  Forest );
  |];
  [|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Desert );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  |];
  [|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Desert );
  Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
  |]
  |])
  in
  
let arr = [|[|0;0;0;0;0;0;0;0|];
            [|0;0;0;0;0;0;0;0|];
            [|0;0;0;0;0;0;0;0|];
            [|0;0;0;0;0;0;0;0|];
            [|0;0;0;0;0;0;0;0|];
            [|0;0;0;0;0;0;0;0|];
            [|0;0;0;0;0;0;0;0|];
            [|0;0;0; 0;0; 0;0;0|]|] in 
proxi arr [(1,1);(1,2);(0,0);(5,5);(0,5);(5,0);(0,4);(3,0)] 6 (-1,-1);
aff arr ;
aff2 (parc_mat arr 8 8 (0,0) mock_map)


 *)
