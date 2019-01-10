open Graph

val chemin_augmentant : int graph -> id -> id -> (id * int) list
val val_chemin : (Graph.id * int) list -> int 
val val_min_chemin : (Graph.id * int) list -> int 
val print_chemin : (Graph.id * int) list -> unit
val arc_augmente : int graph -> id -> id -> int -> int graph                                
