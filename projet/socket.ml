module Socket: S = struct
	type 'a process = (unit -> 'a)

	type 'a in_port  = in_channel
	type 'a out_port = out_channel
	type 'a channel  = ('a in_port * 'a out_port)

	let return v = (fun () -> v)

	let get_my_addr () =
		(Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

	let establish_server sockaddr =
		let domain  = Unix.domain_of_sockaddr sockaddr in
		let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
		let in_sock = Unix.socket domain Unix.SOCK_STREAM 0 in
			Unix.bind sock sockaddr;
			Unix.listen sock 1;
			Unix.connect in_sock sockaddr;
			let (out_sock, _) = Unix.accept sock in
				let in_chan  = Unix.in_channel_of_descr in_sock
				and out_chan = Unix.out_channel_of_descr out_sock in
					(in_chan, out_chan)
				
	let new_channel () =
		let my_addr = get_my_addr () in
		let rec aux port = 
			try
				let sockaddr = (Unix.ADDR_INET (my_addr, port))
				in establish_server sockaddr
			with _ ->
				aux (port + 1)
			in aux 541073
	
	let put v c () =
		Marshal.to_channel c v []

	let get c () =
		Marshal.from_channel c

	let doco l () = 
		let pids = List.map (fun f -> match Unix.fork () with | 0 -> f () | pid -> pid) l in
		List.iter (fun pid -> Unix.waitpid [] pid; ()) pids

	let bind e e' () = 
		let v = e () in 
			e' v ()

	let run e = e ()
end
