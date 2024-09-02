let () =
  let secret = "Key" in
  let data = "Data" in
  let iters = 3 in
  let graph = GM.init 50 20 in

  if not (GM.validate graph) then
    (log_error "Graph validation failed"; exit 1);

  let encoded = Enc.encode iters data graph secret in
  Printf.printf "Encoded: %s\n" encoded;

  let storage = Storage.init () in
  Storage.add storage "encoded_data" encoded;

  if not (Storage.validate_key storage "encoded_data") then
    log_error "Failed to add data to storage";

  match Storage.fetch storage "encoded_data" with
  | Some stored_data -> Printf.printf "Fetched from storage: %s\n" stored_data
  | None -> log_error "Data not found in storage"

  let decoded = Enc.decode iters encoded graph secret in
  Printf.printf "Decoded: %s\n" decoded