open Graph

val chemin_augmentant : int graph -> id -> id -> (id * int) list
val augmente_chemin : (Graph.id * int) list -> int 
val min_augmente_chemin : (Graph.id * int) list -> int 
val print_chemin : (Graph.id * int) list -> unit
                                              
