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

module Server: S = struct
	type 'a process = (unit -> 'a)

	type 'a in_port  = in_channel
	type 'a out_port = out_channel
	type 'a channel  = ('a in_port * 'a out_port)
	let clients = ref 'a channel list

	let return v = (fun () -> v)

	let new_channel () =
		match !clients with 
			| []   -> raise "Not enough client"
			| a::l -> begin clients := l; a end
	
	let put v c () =
		Marshal.to_channel c v []

	let get c () =
		Marshal.from_channel c

	let doco l () = 

	let bind e e' () = 
		let v = e () in 
			e' v ()

	let run e = e ()

	let get_my_addr () =
		(Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

	let init_server_for_client_collecting () =
		let my_addr = get_my_addr () in
		let rec aux port = 
			try
				let sockaddr = (Unix.ADDR_INET (my_addr, port))
				let domain  = Unix.domain_of_sockaddr sockaddr in
				let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
					Unix.bind sock sockaddr;
					Unix.listen sock 3;
					sock 
			with _ ->
				aux (port + 1)
			in aux 84742

	let collect_clients () =
		let ic, oc = new_channel () in
			let sock = init_server_for_client_collecting ()
				let rec aux i =
					if i > 10 then
						begin
						let (s, _) = Unix.accept sock in
							let in_chan  = Unix.in_channel_of_descr s
							and out_chan = Unix.out_channel_of_descr s in
								(in_chan, out_chan) :: aux (i + 1)
						end
					else [] in
		aux 0

	let init_server () = begin clients := collect_clients (); (); end
		
end

module Client: S = struct
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

	let open_connection sockaddr =
	   let domain = Unix.domain_of_sockaddr sockaddr in
	   let sock = Unix.socket domain Unix.SOCK_STREAM 0 
	   in try Unix.connect sock sockaddr ;
			  (Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock)
		 with exn -> Unix.close sock ; raise exn

	let shutdown_connection inchan =
		Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND

	let wait_server_instruction () = 
		let server = "IP IPI PI PIPI" in
		let port = 541073 in
		let server_addr =
			try Unix.inet_addr_of_string server
			with Failure ("inet_addr_of_string") ->
				try (Unix.gethostbyname server).Unix.h_addr_list.(0)
				with Not_found ->
					Printf.eprintf ("%s : Unknown server\n", server);
					exit -1
			in try
				let sockaddr = (Unix.ADDR_INET (server_addr, port)) in
				let domain  = Unix.domain_of_sockaddr sockaddr in
				let sock    = Unix.socket domain Unix.SOCK_STREAM 0 in
				let in_chan, out_chan = open_connection sockaddr in

					let wait_my_function () =
						let fu = Marshal.JENESAISPAS in_chan in	
							fu in_chan out_chan;
							shutdown_connection in_chan
				
			with Failure ("int_of_string") -> Printf.eprintf "bad port number";
			                                  exit -1
end

