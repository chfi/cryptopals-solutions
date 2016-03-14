open Core.Std;;

(* TODO: escape rather than throw away endlines and stuff *)
let encrypt_file fp =
  let file = In_channel.create fp in
  let key = Bytearray.int_array_of_ascii_string "abcdefgh" in
  let lines =
    List.map (In_channel.input_lines file)
      ~f:String.strip
  in
  let unlined = String.concat lines in
  let bytes = Bytearray.int_array_of_ascii_string unlined in
  let cipherints = Xorcrypto.repeating_key_xor bytes key in
  let cipherb64 = Bytearray.base64_string_of_int_array cipherints in
  cipherb64

let find_keysize fp =
  let file = In_channel.create fp in
  let lines =
    List.map (In_channel.input_lines file)
      ~f:String.strip
  in
  let unlined = String.concat lines in
  let bytes = Bytearray.int_array_of_base64_string unlined in

  let keysizes = Frequencyanalysis.find_key_size bytes 48 in
  keysizes

let find_key fp ks =
  let file = In_channel.create fp in
  let lines =
    List.map (In_channel.input_lines file)
      ~f:String.strip
  in
  let unlined = String.concat lines in
  let bytes = Bytearray.int_array_of_base64_string unlined in
  Xorcrypto.find_repeating_xor_key bytes ks



let decrypt_file fp key =
  let file = In_channel.create fp in
  let lines =
    List.map (In_channel.input_lines file)
      ~f:String.strip
  in
  let unlined = String.concat lines in
  let bytes = Bytearray.int_array_of_base64_string unlined in
  let decrypted = Xorcrypto.repeating_key_xor bytes key in
  Bytearray.ascii_string_of_int_array decrypted


let () =
  let filename = "6.txt" in
  let keysizes = List.slice (find_keysize filename) 0 1 in
  let keys = List.map keysizes (fun (ks,_) -> find_key filename ks) in
  let all_decrypted = List.map keys (fun k -> decrypt_file filename k) in
  print_endline (List.hd_exn all_decrypted);
