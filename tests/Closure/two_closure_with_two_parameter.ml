let x = 5 in
let rec add_left a = a + x in
let rec add_right b = b + x in 
let rec maker u v a b =
  u a + v b in
print_int (maker add_left add_right 3 7 )