open Graph

(* trouve un chemin augmentant un graphe *)
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

(* fonction auxiliare de augmente_chemin *)
let rec val_min_chemin = function
  |[] -> max_int
  |(id,x)::rest -> min x (val_min_chemin rest);;

(* calcul la valeur minimale d'un chemin *)
let val_chemin chemin = if chemin == [] then 0
                             else val_min_chemin chemin;;
(* affiche le chemin *)
let rec print_chemin = function 
  | [] -> ()
  | [(a,b)] -> Printf.printf "(%s) \n" a;
  | (a,b)::l -> Printf.printf "(%s)->" a; print_chemin l;;

(* ford fulkerson algorithm *)
(* while not path_not_found *)
(* find augmenting path *)
(* calculate residual graph based on value of said path *)

let get_arc_value = function
  |None -> 0
  |Some n -> n
               
let rec arc_augmente g node1 node2 valeur = let arc_val_f = find_arc g node1 node2 in
                                            let arc_val_b = find_arc g node2 node1 in
                                            match arc_val_f with
                                            |Some avf -> (let avb = (get_arc_value arc_val_b) in
                                                          (if (avf - valeur) = 0 then add_arc (del_arc g node1 node2) node2 node1 (avb + valeur) (* only add back arc and delete forward arc *)
                                                           else add_arc (add_arc g node1 node2 (avf - valeur)) node2 node1 (avb + valeur)))
                                            |None -> g;;

let rec graphe_augmente g ch valeur = match ch with
                                      |[] -> g
                                      |[(id,x)] -> g
                                      |(id1, x1)::(id2, x2)::chemin -> let nouv_graphe = (arc_augmente g id1 id2 valeur)  in
                                                                       graphe_augmente nouv_graphe ((id2, x2)::chemin) valeur;;


(* calculate the sum of the out_arc labels that go to the sink*)
let rec graphe_flow gr s = v_fold gr (fun acu id out_arcs -> let v = get_arc_value (find_arc gr id s) in if v != 0 then acu+v else acu) 0;; 


let ford_fulkerson graphe source puits = let false_flow = (graphe_flow graphe source) in                            
                            let rec ford_fulkerson_aux gr s p = let next_path = chemin_augmentant gr s p in
                                                                match next_path with
                                                                |[] -> (gr, (graphe_flow gr s) - false_flow) 
                                                                |_ -> ford_fulkerson_aux (graphe_augmente gr next_path (val_chemin next_path)) s p
                            in ford_fulkerson_aux graphe source puits;; 


