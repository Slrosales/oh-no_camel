let a = Array.create 1 (let rec id x = x in id) in
(a.(0)) 5