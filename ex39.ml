let rec addOne = function
  | [] -> []
  | h::t -> h+1::addOne t

let _ = List.iter (Printf.printf "%d ") (addOne [1;2;3;4;5])