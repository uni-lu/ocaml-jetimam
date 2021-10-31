let sum n =
  let rec tail n acc =
    if n = 0 then
      acc
    else
      tail (n-1) (acc+n) in tail n 0

let rec print n m =
  if n = 0 then
    Printf.printf "= %d" m
  else
    Printf.printf "%d + " n
    print n-1 m
      
let _ =
  let x = read_int () in 
  let y = sum x in
  let out = print "%d" x y in
  let _ = Printf.printf "%s\n" out in ()