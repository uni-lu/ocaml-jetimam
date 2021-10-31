let reverse l =
  let rec tail acc = function
    | [] -> acc
    | h::t -> tail (h::acc) t
  in tail [] l

let append l1 l2 =
  let rec loop l1 = function
    | [] -> l1
    | h::t -> loop (h::l1) t
    in loop l1 l2

let _ = 
  let l1 = [1;2;3] in
  let l2 = [4;5;6] in
  List.iter (Printf.printf "%d ") (append (reverse l1) l2)