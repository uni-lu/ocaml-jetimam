let fact n =
	let rec tail n acc =
		if n = 0 then
			acc
		else
			tail (n-1) n*acc
	in tail n 1

let _ =
	let x = read_int () in

	let y = fact x in

	Printf.printf "%d\n" y