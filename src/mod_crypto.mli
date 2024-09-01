module type NODE = sig
    type t
    val make : int -> t
    val weight : t -> float
    val state_sum : t -> float
    val combine : t -> t -> t
    val identity : t
  end
  
  module Node : NODE
  
  module type GRAPH = sig
    type node
    type t
    val init : int -> int -> t
    val find : t -> int -> node option
    val ids : t -> int list
    val combine : t -> t -> t
    val identity : t
  end
  
  module GraphModule : functor (N : NODE) -> GRAPH with type node = N.t
  
  module Encrypt : functor (G : GRAPH with type node = Node.t) -> sig
    val encode : int -> string -> G.t -> string -> string
    val decode : int -> string -> G.t -> string -> string
  end
  