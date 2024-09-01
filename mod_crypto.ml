open Random
open Printf

(* Helper function to enable option chaining (f? also check...) *)
let ( >>= ) option f = match option with
  | Some x -> f x
  | None -> None

module type NODE = sig
  type t
  val make : int -> t
  val weight : t -> float
  val state_sum : t -> float
  val combine : t -> t -> t
  val identity : t
end

module Node : NODE = struct
  type t = {
    id : int;
    attrs : (string * float) list;
    state : float array array;
    links : (int * float) list;
  }
  (* Check List!!!! *)
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
    Hashtbl.iter (fun k v -> Hashtbl.replace nodes k (N.combine v (Hashtbl.find g2.nodes k))) g2.nodes;
    let edges = HS.union g1.edges g2.edges in
    { nodes; edges }

  let identity = { nodes = Hashtbl.create 0; edges = HS.empty }
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

let () =
  let secret = "Key" in
  let data = "Data" in
  let iters = 3 in
  let g = GM.init 50 20 in
  let encoded = Enc.encode iters data g secret in
  Printf.printf "Encoded: %s\n" encoded;
  (* Decode -0x + N (???) *)
  let decoded = Enc.decode iters encoded g secret in
  Printf.printf "Decoded: %s\n" decoded
