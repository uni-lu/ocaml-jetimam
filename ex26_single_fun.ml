let rec count_down n =
	if n = 0 then
		"0"
	else if n = 1 then
		"1\n1"
	else
		Printf.sprintf "%d\n%s\n%d" n (count_down(n-1)) n

let _ =
	let x = Scanf.scanf "%d" (fun x -> x) in

	let y = count_down x in

	let _ = Printf.printf "%s\n" y in ()