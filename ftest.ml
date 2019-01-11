open Graph
open Ford_fulkerson

let () =

  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf "\nUsage: %s infile source sink printRaw outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;
  (* Outfile is the graph raw, and the Outfile2 is graph after the algo*)
  let infile = Sys.argv.(1)
  and printinfile = Sys.argv.(4)
  and outfile = Sys.argv.(5)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  (* Just print the raw graph that has been read.*)
  let () = Gfile.write_file printinfile graph in
  let () = Gfile.export printinfile graph in
	
  let graph = map graph (fun x -> (int_of_string x)) in
  let (gr, soln) = ford_fulkerson graph _source _sink in  

  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile gr in
  let () = Gfile.export outfile gr in
  Printf.printf "Value of flow is %d\n" soln;;


