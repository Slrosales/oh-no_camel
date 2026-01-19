let x = 2 in
let rec add_5_time_x z =
  ((((z + x)+x)+x)+x)+x in
print_int (add_5_time_x x)