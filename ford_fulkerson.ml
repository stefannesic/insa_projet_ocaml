open Graph

let chemin_augmentant gr s p = let rec trouve_chemin chemin arcs =
                                 match arcs with
                                 |[] -> []
                                 |(id, x)::rest -> if List.mem (id,x) chemin then trouve_chemin chemin rest (* si noeud est déjà dans le chemin, pour éviter cycles *)
                                                   else 
                                                     (if id = p then (id, x)::chemin
                                                      else
                                                        let c = (trouve_chemin ((id, x)::chemin) (out_arcs gr id)) in
                                                        if c = [] then trouve_chemin chemin rest 
                                                        else c)
                               in List.rev (trouve_chemin [(s, max_int)] (out_arcs gr s));;

let rec min_augmente_chemin = function
  |[] -> max_int
  |(id,x)::rest -> min x (min_augmente_chemin rest);;

let augmente_chemin chemin = if chemin == [] then 0
                             else min_augmente_chemin chemin;;

let rec print_chemin = function 
  | [] -> ()
  | [(a,b)] -> Printf.printf "(%s) \n" a;
  | (a,b)::l -> Printf.printf "(%s)->" a; print_chemin l;;




