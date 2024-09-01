let () =
  let secret = "Key" in
  let data = "Data" in
  let iters = 3 in
  let module Node = Mod_crypto.Node in
  let module GM = Mod_crypto.GraphModule(Node) in
  let module Enc = Mod_crypto.Encrypt(GM) in
  let g = GM.init 50 20 in
  let encoded = Enc.encode iters data g secret in
  Printf.printf "Encoded: %s\n" encoded;
  let decoded = Enc.decode iters encoded g secret in
  Printf.printf "Decoded: %s\n" decoded