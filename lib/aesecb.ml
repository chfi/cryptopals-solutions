open Core.Std
open Cryptokit

(* plaintext and key is string! *)
let decrypt_ecb plaintext key =
  let bytes = Bytearray.int_array_of_ascii_string plaintext in
  let module C = Cryptokit.Cipher in
  let decrypt_transform = C.aes ~mode:C.ECB key C.Decrypt in
  ignore (Array.map ~f:(decrypt_transform#put_byte) bytes);
  decrypt_transform#get_string

let encrypt_ecb plaintext key =
  let bytes = Bytearray.int_array_of_ascii_string plaintext in
  let module C = Cryptokit.Cipher in
  let encrypt_transform = C.aes ~mode:C.ECB key C.Encrypt in
  ignore (Array.map ~f:(encrypt_transform#put_byte) bytes);
  encrypt_transform#get_string
