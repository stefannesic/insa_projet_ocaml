open Graph

(* ajouter check si noeud est déjà dans le chemin*)
let chemin_augmentant gr s p = let rec trouve_chemin chemin arcs =
                                 match arcs with
                                 |[] -> []
                                 |(id, x)::rest -> if id = p then (id, x)::chemin
                                                   else
                                                     let c = (trouve_chemin ((id, x)::chemin) (out_arcs gr id)) in
                                                     if c = [] then trouve_chemin chemin rest 
                                                     else c
                               in List.rev (trouve_chemin [(s, max_int)] (out_arcs gr s));;   




