open Mapgen;;

let random_map = gen_random_values 50;;
print_int_map random_map;;
print_char '\n';;
print_int_map (interpolate random_map);;

