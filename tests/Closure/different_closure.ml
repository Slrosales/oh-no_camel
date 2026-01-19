let rec adder x =
  let rec add_x y = y + x in
  add_x in
let rec suber x = 
  let rec sub_x y = y + x in
  sub_x in
let not_used_var = print_int ((adder 5) 15) in
print_int ((suber 5) 15) 