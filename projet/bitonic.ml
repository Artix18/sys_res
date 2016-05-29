open Obj

module Bitonic (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  (*let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2
    *)

  let read_tab () =
  	let n = read_int() in
  	let tab = Array.make n 0 in
  	for i = 0 to (n-1) do
  		tab.(i) <- read_int()
  	done;
  	tab
  
  let glb_tab = read_tab ()
  
  let bitonic_compare sens tab = 
  	let dist = (Array.length tab) / 2 in
  	for i=0 to (dist-1) do
  		if (tab.(i) > tab.(i+dist)) = sens then
  		(
  			let a = tab.(i) in
  			tab.(i) <- tab.(i+dist);
  			tab.(i+dist) <- a
  		)
  	done
  
  let rec bitonic_merge sens tab = 
  	let n = Array.length tab in
  	if n = 1 then
  		tab
  	else
  	(
  		bitonic_compare sens tab;
  		let gauche = bitonic_merge sens (Array.sub tab 0 (n/2)) in
  		let droite = bitonic_merge sens (Array.sub tab (n/2) (n-1)) in
  		Array.append gauche droite
  	)

	(* todo : paralleliser ici, a priori je crois qu'il faut juste doco *)
  let rec bitonic_sort sens tab = 
  	let n = Array.length tab in
  	if n <= 1 then K.doco []
  	else
  	(
  		let gauche_tc : (unit K.process) = Obj.magic (fun () -> bitonic_sort true (Array.sub tab 0 (n/2)); ()) in
  		let droite_tc = Obj.magic (fun () -> bitonic_sort false (Array.sub tab (n/2) (n-1)); ()) in
  		
  		K.doco [gauche_tc; droite_tc];
  		bitonic_merge sens tab (*normalement ça n'a pas recopie les tab donc ok*);
  		K.doco []
  	)

  (*
  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()*)
    
  let output () = 
  	let n = Array.length glb_tab in
  	for i = 0 to (n-1) do
  		print_int(glb_tab.(i))
  	done;
  	K.doco []

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ bitonic_sort true glb_tab; output () ; ])

end

module E = Bitonic(Kahn.Th)

let () = E.K.run E.main