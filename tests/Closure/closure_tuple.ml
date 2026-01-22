let tuple = (5,3,2) in
let global_x = 2 in
let rec somme_tuple_iter a = 
  let tmp = global_x in 
  let (x,y,z) = tuple in
  
  (tmp+x+a,tmp+y+a,tmp+z+a) in
let (x1,y1,z1) = somme_tuple_iter 5 in
let _ = print_int x1 in
let _ = print_int y1 in
print_int z1