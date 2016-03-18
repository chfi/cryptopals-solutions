open Core.Std

let decrypt_file fp key =
  let file = In_channel.create fp in
  let input = String.concat (List.map (In_channel.input_lines file) ~f:String.strip) in
  let bytes = Bytearray.int_array_of_base64_string input in
  let key_bytes = Bytearray.int_array_of_ascii_string key in
  let iv = Array.init 16 ~f:(fun _ -> 0) in
  let decrypted = Aes.decrypt_cbc bytes key_bytes iv in
  (Bytearray.ascii_string_of_int_array decrypted)

let () =
  let filename = "10.txt" in
  let results = decrypt_file filename "YELLOW SUBMARINE" in
  print_endline results
