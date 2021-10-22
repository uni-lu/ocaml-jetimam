let ask_number_to_user () =
  read_int()

let count_points input =

  let x = Random.int 7 in
  let y = Random.int 7 in
  let z = Random.int 7 in

  if input > x && input > y || input > x && input > z || input > y && input > z then
    2
  else if input = x || input = y || input = z then
    1
  else
    0

let print_points_earned =
  Printf.printf "%d\n"

let rec round score = 
  let input = ask_number_to_user () in 
  let new_score = score + count_points input in
  let _ = print_points_earned (new_score) in
  round new_score

let _ =
  let _ = Random.self_init() in
  round 0