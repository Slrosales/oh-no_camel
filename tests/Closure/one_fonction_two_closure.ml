let z = 5 in
let rec adder x = 
  let rec add_x y = y + z + x in
  let rec sub_x y = y + (z - x) in
  add_x z + sub_x z in
print_int (adder 5)