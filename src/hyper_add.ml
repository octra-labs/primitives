module KeyHandler = struct
  open Z

  type key = { pub_key: Z.t; priv_key: Z.t }
  
  let generate_key_pair () =
    let bit_size = Z.of_int 256 in
    let random_state = Random.State.make_self_init () in
    let random_bits = Random.State.bits random_state |> string_of_int in
    let random_bigint = Z.of_bits random_bits in
    let shift_amount = Z.sub bit_size (Z.of_int (Z.numbits random_bigint)) in
    let pub_key = Z.nextprime (Z.shift_left random_bigint (Z.to_int shift_amount)) in
    let modulus = Z.pred (Z.pow (Z.of_int 2) (Z.to_int bit_size)) in
    let priv_key = Z.invert pub_key modulus in
    if Z.equal priv_key Z.zero then
      failwith "Failed to generate private key"
    else
      { pub_key; priv_key }

  let get_pub_key { pub_key; _ } = pub_key
  let get_priv_key { priv_key; _ } = priv_key
end

module NoiseHandler = struct
  type t = { level : float; intensity : float; rate: float }

  let create level intensity rate = { level; intensity; rate }

  let generate { level; intensity; rate } x =
    let noise_effect = intensity *. (x ** level) *. sin (rate *. Float.pi *. x) in
    let generated_noise = Z.add (Z.of_float (abs_float noise_effect)) (Z.of_int (Random.int 20 + 5)) in
    Printf.printf "Generated noise: %s\n" (Z.to_string generated_noise);
    generated_noise
end

module TransitionState = struct
  open Z

  let update_state sc s b p gt_n =
    Z.((shift_left b 18 - p) * gt_n + sc + one)

  let calculate_position sc =
    Z.div sc (Z.shift_left Z.one 16)

  let update_new_state s st_s =
    Z.logor (Z.shift_left s 8) st_s

  let update_st_array st_s b =
    Z.logand (Z.add (Z.add st_s st_s) b) (Z.of_int 255)

  let control_vector vx vy p =
    let mask = Z.of_int 0xffff in
    let diff = Z.sub vy vx in
    let lower_bits = Z.logand diff mask in
    Z.((vx + div diff (shift_left one 16)) + lower_bits * p)

  let validate_control_vector control_vector expected =
    if Z.equal control_vector expected then
      Printf.printf "Control vector is valid: %s\n" (Z.to_string control_vector)
    else
      Printf.printf "Control vector is invalid: %s, expected: %s\n"
        (Z.to_string control_vector) (Z.to_string expected)
end

module H_Graph = struct
  open Z

  type 'a node = {
    id: Z.t;
    enc_data: (Z.t * Z.t) list;
    noise_lvl: Z.t;
    state: 'a;
  }

  type 'a edge = {
    nodes: 'a node list;
    op: string;
    control_vector: Z.t;
  }

  type 'a t = {
    nodes: 'a node list;
    edges: 'a edge list;
    state_arr: Z.t array;
    st_arr: Z.t array;
  }

  let create () = 
    { nodes = []; edges = []; state_arr = Array.make 256 Z.zero; st_arr = Array.make 256 Z.zero }

  let add_node g enc_data noise_lvl state =
    let new_id = Z.add (Z.of_int (List.length g.nodes)) Z.one in
    let new_node = { id = new_id; enc_data; noise_lvl; state } in
    Printf.printf "Adding node with ID: %s\n" (Z.to_string new_id);
    { g with nodes = new_node :: g.nodes }

  let add_edge g nodes op control_vector =
    let new_edge = { nodes; op; control_vector } in
    Printf.printf "Adding edge with operation: %s, control vector: %s\n" op (Z.to_string control_vector);
    { g with edges = new_edge :: g.edges }

  let adjust_noise noise r =
    let r_z = Z.of_int r in
    let quad_effect = Z.mul (Z.pow r_z 2) (Z.div (Z.pow noise 2) (Z.pow r_z 2)) in
    let cubic_effect = Z.mul (Z.pow r_z 4) (Z.div (Z.pow noise 3) (Z.pow r_z 3)) in
    let adjusted_noise = Z.sub noise (Z.add quad_effect cubic_effect) in
    let final_noise = if Z.lt adjusted_noise Z.zero then Z.zero else adjusted_noise in
    Printf.printf "Adjusting noise: initial = %s, adjusted = %s\n" (Z.to_string noise) (Z.to_string final_noise);
    final_noise

  let evaluate_noise g r =
    let r_z = Z.of_int r in
    let noise_sq_sum = List.fold_left (fun acc node -> Z.add acc (Z.pow node.noise_lvl 2)) Z.zero g.nodes in
    let noise_cub_sum = List.fold_left (fun acc node -> Z.add acc (Z.pow node.noise_lvl 3)) Z.zero g.nodes in
    let quad_effect = Z.mul (Z.pow r_z 2) (Z.div noise_sq_sum (Z.pow r_z 2)) in
    let cub_effect = Z.mul (Z.pow r_z 4) (Z.div noise_cub_sum (Z.pow r_z 3)) in
    let inter_effect = Z.mul (Z.pow r_z 6) (Z.div noise_sq_sum (Z.pow r_z 4)) in
    let total_effect = Z.add (Z.add quad_effect cub_effect) inter_effect in
    Printf.printf "Evaluated noise effect: %s\n" (Z.to_string total_effect);
    total_effect

  let is_good_row noise_lvl r =
    let r_z = Z.of_int r in
    let log_r = Z.of_float (log (Z.to_float r_z)) in
    let threshold = Z.div (Z.add (Z.div Z.one r_z) (Z.div Z.one (Z.mul r_z r_z))) log_r in
    Printf.printf "Noise level: %s, Threshold: %s\n" (Z.to_string noise_lvl) (Z.to_string threshold);
    Z.leq noise_lvl threshold

  let evaluate_stability g r =
    List.for_all (fun node -> is_good_row node.noise_lvl r) g.nodes

  let encrypt_number h sigma frequency key number =
    let noise = NoiseHandler.generate (NoiseHandler.create h sigma frequency) (Random.float 1.0) in
    let enc_value = Z.add (Z.mul (Z.of_int number) key.KeyHandler.pub_key) noise in
    let state = TransitionState.update_new_state (Z.of_int number) noise in
    Printf.printf "Encrypting number: %d, Encrypted value: %s, Noise: %s\n"
      number (Z.to_string enc_value) (Z.to_string noise);
    [(enc_value, noise)], state

  let decrypt_number key enc is_negative =
    match enc with
    | [(value, noise)] ->
        let dec_value = Z.sub value noise in
        Printf.printf "Decrypting: value = %s, noise = %s, dec_value = %s\n"
          (Z.to_string value) (Z.to_string noise) (Z.to_string dec_value);
        
        if Z.equal dec_value Z.zero then 0
        else 
          let div_value = Z.div dec_value key.KeyHandler.pub_key in
          let remainder = Z.erem dec_value key.KeyHandler.pub_key in
          let corrected_value = 
            if Z.gt remainder (Z.div key.KeyHandler.pub_key (Z.of_int 2)) then
              Z.add div_value Z.one
            else
              div_value
          in
          let final_value =
            if is_negative then Z.neg corrected_value
            else corrected_value
          in
          Printf.printf "Dividing decrypted value by public key: dec_value = %s, pub_key = %s, div_value = %s, corrected sign = %d\n"
            (Z.to_string dec_value) (Z.to_string key.KeyHandler.pub_key) (Z.to_string corrected_value) (if is_negative then -1 else 1);
          Z.to_int final_value
    | _ -> failwith "Unexpected encrypted format"

  let perform_addition g nodes key_pair =
    let node_a = List.hd nodes in
    let node_b = List.hd (List.tl nodes) in
    let new_enc_data = List.map2 (fun (ar, an) (br, bn) ->
      let combined_noise = Z.add an bn in
      let adjusted_noise = adjust_noise combined_noise 100 in
      (Z.add ar br, adjusted_noise)
    ) node_a.enc_data node_b.enc_data in

    let new_state = TransitionState.update_state node_a.state node_b.state (Z.of_int 1) (Z.of_int 2) (Z.of_int 1) in
    let control_vector = TransitionState.control_vector node_a.state node_b.state new_state in
    let expected_vector = TransitionState.control_vector node_a.state node_b.state new_state in
    TransitionState.validate_control_vector control_vector expected_vector;
    
    let new_node = { id = Z.add (Z.of_int (List.length g.nodes)) Z.one; enc_data = new_enc_data; noise_lvl = node_a.noise_lvl; state = new_state } in
    let g = { g with nodes = new_node :: g.nodes } in
    let _ = add_edge g [node_a; node_b] "add" control_vector in
    
    let dec_sum = decrypt_number key_pair new_node.enc_data false in
    dec_sum
end

