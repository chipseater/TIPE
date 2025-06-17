open Game

let _ =
  let x = Unix.time () in
  game 1 10;
  print_char '\n';
  print_int (int_of_float (Unix.time ()) - int_of_float x)
