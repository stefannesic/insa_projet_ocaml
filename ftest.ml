open Graph
open Ford_fulkerson

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  (*let aug_graph = map graph (fun x -> (int_of_string x)) in
  let aug_graph = arc_augmente aug_graph "4" "5" 3 in
  let aug_graph = map aug_graph (fun x -> (string_of_int x)) in*)
	
  let graph = map graph (fun x -> (int_of_string x)) in
  let (gr, soln) = ford_fulkerson graph _source _sink in  
  let gr = map gr (fun x -> (string_of_int x)) in

  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile gr in
  let () = Gfile.export outfile gr in
  Printf.printf "Value of flow is %d\n" soln;;


  (*let chemin = chemin_augmentant graph "0" "5"
  in
  print_chemin chemin;
  Printf.printf "Min value is %d\n" (val_chemin chemin);;*)


