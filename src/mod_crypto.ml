open Printf

let ( >>= ) option f = match option with
  | Some x -> f x
  | None -> None

let log_event event =
  let timestamp = Unix.time () |> Unix.gmtime in
  let time_str = Printf.sprintf "[%02d:%02d:%02d]" timestamp.tm_hour timestamp.tm_min timestamp.tm_sec in
  Printf.printf "%s %s\n%!" time_str event

let log_error error =
  Printf.printf "[ERROR] %s\n%!" error

module type NODE = sig
  type t
  val make : int -> t
  val weight : t -> float
  val state_sum : t -> float
  val combine : t -> t -> t
  val validate : t -> bool
  val identity : t
end

module Node : NODE = struct
  type t = {
    id : int;
    attrs : (string * float) list;
    state : float array array;
    links : (int * float) list;
  }
  
  let make id =
    let attrs = List.init (Random.int 10) (fun i -> ("Attr" ^ string_of_int i, Random.float 100.0)) in
    let state = Array.init 4 (fun _ -> Array.init 4 (fun _ -> Random.float 1.0)) in
    let links = List.init (Random.int 5) (fun _ -> (Random.int 1000000, Random.float 10.0)) in
    { id; attrs; state; links }

  let weight n = List.fold_left (fun acc (_, w) -> max acc w) 0.0 n.links

  let state_sum n = Array.fold_left (fun acc row -> acc +. Array.fold_left (+.) 0.0 row) 0.0 n.state
  
  let combine n1 n2 = 
    { id = n1.id + n2.id;  
      attrs = n1.attrs @ n2.attrs;
      state = Array.map2 (Array.map2 (+.)) n1.state n2.state;
      links = n1.links @ n2.links }

  let validate n =
    try
      let _ = List.fold_left (fun acc (_, w) -> acc +. w) 0.0 n.links in
      Array.iter (fun row -> ignore (Array.fold_left (+.) 0.0 row)) n.state;
      true
    with _ -> false

  let identity = { id = 0; attrs = []; state = Array.make_matrix 4 4 0.0; links = [] }
end

module type GRAPH = sig
  type node
  type t
  val init : int -> int -> t
  val find : t -> int -> node option
  val ids : t -> int list
  val combine : t -> t -> t
  val identity : t
  val validate : t -> bool
  val to_string : t -> string
end

module GraphModule (N : NODE) : GRAPH with type node = N.t = struct
  module NS = Set.Make(Int)
  module HS = Set.Make(NS)

  type node = N.t

  type t = {
    nodes : (int, node) Hashtbl.t;
    edges : HS.t;
  }

  let init n_nodes n_edges =
    let nodes = Hashtbl.create n_nodes in
    let edges = 
      List.init n_edges (fun _ -> NS.of_list (List.init 3 (fun _ -> Random.int 1000000)))
      |> List.fold_left (fun acc edge -> HS.add edge acc) HS.empty 
    in
    List.iter (fun _ ->
        let id = Random.int 1000000 in
        Hashtbl.add nodes id (N.make id)
      ) (List.init n_nodes (fun _ -> ()));
    { nodes; edges }

  let find g id = Hashtbl.find_opt g.nodes id

  let ids g = Hashtbl.fold (fun k _ acc -> k :: acc) g.nodes []

  let combine g1 g2 =
    let nodes = Hashtbl.copy g1.nodes in
    Hashtbl.iter (fun k v -> 
      match Hashtbl.find_opt g2.nodes k with
      | Some v2 -> Hashtbl.replace nodes k (N.combine v v2)
      | None -> Hashtbl.add nodes k v
    ) g1.nodes;
    let edges = HS.union g1.edges g2.edges in
    { nodes; edges }

  let identity = { nodes = Hashtbl.create 0; edges = HS.empty }

  let validate g =
    Hashtbl.fold (fun _ node valid ->
      valid && N.validate node && List.for_all (fun (id, _) -> Hashtbl.mem g.nodes id) node.links
    ) g.nodes true

  let to_string g =
    Hashtbl.fold (fun id node acc ->
      acc ^ Printf.sprintf "Node %d: weight = %.2f, state_sum = %.2f\n" id (N.weight node) (N.state_sum node)
    ) g.nodes ""
end

module type STORAGE = sig
  type t
  val init : unit -> t
  val add : t -> string -> string -> unit
  val fetch : t -> string -> string option
  val list : t -> string list
  val validate_key : t -> string -> bool
end

module InMemoryStorage : STORAGE = struct
  type t = (string, string) Hashtbl.t

  let init () = Hashtbl.create 100

  let add storage key value =
    Hashtbl.replace storage key value;
    log_event (Printf.sprintf "Added entry with key: %s" key)

  let fetch storage key =
    Hashtbl.find_opt storage key

  let list storage =
    Hashtbl.fold (fun k _ acc -> k :: acc) storage []

  let validate_key storage key =
    Hashtbl.mem storage key
end

module Encrypt (G : GRAPH with type node = Node.t) = struct
  let encode rounds data graph secret =
    let secret_arr = Array.of_list (List.init (String.length secret) (String.get secret)) in
    let node_ids = G.ids graph in
    let node_count = List.length node_ids in
    let secret_len = Array.length secret_arr in

    let apply_encoding c round_idx char_idx =
      let key_char = secret_arr.((char_idx + round_idx) mod secret_len) in
      let idx = (Char.code c + round_idx + char_idx) mod node_count in
      List.nth_opt node_ids idx >>= (fun id -> G.find graph id)
      |> Option.map (fun node ->
          let wt = Node.weight node in
          let sum = Node.state_sum node +. wt in
          let result = (Char.code c) lxor (Char.code key_char) lxor (int_of_float sum land 0xFF) in
          Printf.printf "Encoding char '%c' using node %d: wt=%.2f sum=%.2f result=%c\n" c idx wt sum (Char.chr result);
          Char.chr result)
      |> Option.value ~default:c
    in

    let map_with_index f seq =
      fst (Seq.fold_left (fun (acc, idx) c -> (Seq.append acc (Seq.return (f c idx)), idx + 1)) (Seq.empty, 0) seq)
    in

    List.init rounds (fun round_idx ->
        data
        |> String.to_seq
        |> map_with_index (fun c char_idx -> apply_encoding c round_idx char_idx)
        |> List.of_seq
      )
    |> List.flatten
    |> List.fold_left (fun acc c -> acc ^ sprintf "%02x" (Char.code c)) ""

  let decode rounds encoded_data graph secret =
    let secret_arr = Array.of_list (List.init (String.length secret) (String.get secret)) in
    let node_ids = G.ids graph in
    let node_count = List.length node_ids in
    let secret_len = Array.length secret_arr in

    let chars = List.init (String.length encoded_data / 2) (fun i ->
        int_of_string ("0x" ^ String.sub encoded_data (2 * i) 2)
      ) in

    let apply_decoding code round_idx char_idx =
      let key_char = secret_arr.((char_idx + round_idx) mod secret_len) in
      let idx = (Char.code (Char.chr code) + round_idx + char_idx) mod node_count in
      List.nth_opt node_ids idx >>= (fun id -> G.find graph id)
      |> Option.map (fun node ->
          let wt = Node.weight node in
          let sum = Node.state_sum node +. wt in
          let result = (code lxor (Char.code key_char) lxor (int_of_float sum land 0xFF)) in
          Printf.printf "Decoding code %02x using node %d: wt=%.2f sum=%.2f result=%c\n" code idx wt sum (Char.chr result);
          Char.chr result)
      |> Option.value ~default:(Char.chr code)
    in

    let map_with_index f seq =
      fst (Seq.fold_left (fun (acc, idx) c -> (Seq.append acc (Seq.return (f c idx)), idx + 1)) (Seq.empty, 0) seq)
    in

    List.init rounds (fun round_idx ->
        chars
        |> List.to_seq
        |> map_with_index (fun c char_idx -> apply_decoding c round_idx char_idx)
        |> List.of_seq
      )
    |> List.flatten
    |> List.fold_left (fun acc c -> acc ^ String.make 1 c) ""
end

module GM = GraphModule(Node)
module Enc = Encrypt(GM)
module Storage = InMemoryStorage