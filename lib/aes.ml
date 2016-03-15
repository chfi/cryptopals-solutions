open Core.Std
open Cryptokit


(* block and key are int arrays;
   we assume block is padded if necessary! *)
let crypt_single_block_ecb ~dir block key =
  let plaintext_string = Bytearray.ascii_string_of_int_array block in
  let key_string = Bytearray.ascii_string_of_int_array key in

  let module C = Cryptokit.Cipher in

  let transform = C.aes ~mode:C.ECB key_string dir in
  transform#put_string plaintext_string;
  transform#finish;
  Bytearray.int_array_of_ascii_string (transform#get_string)


let encrypt_single_block_cbc prev_block cur_block key =
  let module C = Cryptokit.Cipher in
  let xored_block = Xorcrypto.xor prev_block cur_block in
  crypt_single_block_ecb ~dir:C.Encrypt xored_block key

let decrypt_single_block_cbc prev_block cur_block key =
  let module C = Cryptokit.Cipher in
  let decrypted_block = crypt_single_block_ecb ~dir:C.Decrypt cur_block key in
  Xorcrypto.xor prev_block decrypted_block


(* ciphertext and key are int arrays,
   TODO: make it possible to change padding mode *)
let decrypt_ecb ciphertext key =
  let unpadder = Bytearray.unpad_int_array_pkcs7 16 in
  let blocks = Bytearray.split_every_n ciphertext 16 in
  let decrypted_blocks =
    List.map blocks ~f:(fun block ->
        crypt_single_block_ecb
          ~dir:Cryptokit.Cipher.Decrypt block key)
  in
  let decrypted_array = Array.concat decrypted_blocks in
  unpadder decrypted_array

(* ciphertext and key are int arrays,
   TODO: make it possible to change padding mode *)
let encrypt_ecb plaintext key =
  let padder = Bytearray.pad_int_array_pkcs7 16 in
  let padded = padder plaintext in
  (* let padded = plaintext in *)
  let blocks = Bytearray.split_every_n padded 16 in
  let encrypted_blocks =
    List.map blocks ~f:(fun block ->
        crypt_single_block_ecb
          ~dir:Cryptokit.Cipher.Encrypt block key)
  in
  Array.concat encrypted_blocks

let decrypt_cbc ciphertext key iv =
  let unpadder = Bytearray.unpad_int_array_pkcs7 16 in
  let blocks = Bytearray.split_every_n ciphertext 16 in
  let decrypted_blocks =
    List.mapi blocks ~f:(fun i cur_block ->
        let prev_block =
          if i = 0 then iv else (List.nth_exn blocks (i-1))
        in
        decrypt_single_block_cbc
          prev_block cur_block key
      )
  in
  let decrypted_array = Array.concat decrypted_blocks in
  unpadder decrypted_array

let encrypt_cbc plaintext key iv =
  let padder = Bytearray.pad_int_array_pkcs7 16 in
  let padded = padder plaintext in
  let blocks = Bytearray.split_every_n padded 16 in
  let temp_block = Array.copy iv in

  let encrypted_blocks =
    List.map blocks ~f:(fun cur_block ->
        (* keep track of the last encrypted block *)
        let crypted = encrypt_single_block_cbc temp_block cur_block key in
        Array.iteri crypted ~f:(fun i b -> temp_block.(i) <- b);
        crypted
      )
  in
  Array.concat encrypted_blocks
