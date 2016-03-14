open Core.Std
open Cryptokit

let decrypt_file fp key =
  let file = In_channel.create fp in
  let lines =
    List.map (In_channel.input_lines file)
      ~f:String.strip
  in
  let unlined = String.concat lines in
  let bytes = Bytearray.int_array_of_base64_string unlined in

  let decrypt_transform = Cryptokit.Cipher.aes
    ~mode:Cryptokit.Cipher.ECB
    key Cryptokit.Cipher.Decrypt
  in

  decrypt_transform#put_string unlined;
  decrypt_transform#finish;
  ignore (Array.map ~f:(decrypt_transform#put_byte) bytes);

  let decrypted = decrypt_transform#get_string in
  decrypted

let () =
  let filename = "7.txt" in
  let result = decrypt_file filename "YELLOW SUBMARINE" in
  print_endline result
