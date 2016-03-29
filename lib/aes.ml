open Core.Std
open Cryptokit

(* block and key are int arrays;
   we assume block is padded if necessary! *)
let crypt_single_block_ecb ~dir key block =
  let module C = Cryptokit.Cipher in
  let plaintext_string = Bytearray.ascii_string_of_int_array block in
  let key_string = Bytearray.ascii_string_of_int_array key in
  let transform = C.aes ~mode:C.ECB key_string dir in
  transform#put_string plaintext_string;
  transform#finish;
  Bytearray.int_array_of_ascii_string (transform#get_string)


let encrypt_single_block_cbc key prev_block cur_block =
  let module C = Cryptokit.Cipher in
  Xorcrypto.xor prev_block cur_block
  |> crypt_single_block_ecb ~dir:C.Encrypt key


let decrypt_single_block_cbc key prev_block cur_block =
  let module C = Cryptokit.Cipher in
  crypt_single_block_ecb ~dir:C.Decrypt key cur_block
  |> Xorcrypto.xor prev_block


(* ciphertext and key are int arrays,
   TODO: make it possible to change padding mode *)
let decrypt_ecb ciphertext key =
  let module C = Cryptokit.Cipher in
  let unpadder = Bytearray.unpad_int_array_pkcs7 in
  Bytearray.split_every_n ciphertext 16
  |> List.map ~f:(fun b ->
      crypt_single_block_ecb ~dir:C.Decrypt key b)
  |> Array.concat
  |> unpadder


(* ciphertext and key are int arrays,
   TODO: make it possible to change padding mode *)
let encrypt_ecb plaintext key =
  let module C = Cryptokit.Cipher in
  let padder = Bytearray.pad_int_array_pkcs7 16 in
  let padded = padder plaintext in
  Bytearray.split_every_n padded 16
  |> List.map ~f:(fun b ->
      crypt_single_block_ecb ~dir:C.Encrypt key b)
  |> Array.concat


let decrypt_cbc ciphertext key iv =
  let unpadder = Bytearray.unpad_int_array_pkcs7 in
  let blocks = Bytearray.split_every_n ciphertext 16 in
  List.mapi ~f:(fun i cur_block ->
        let prev_block =
          if i = 0 then iv else (List.nth_exn blocks (i-1))
        in
        decrypt_single_block_cbc key prev_block cur_block)
    blocks
  |> Array.concat
  |> unpadder


let encrypt_cbc plaintext key iv =
  let padder = Bytearray.pad_int_array_pkcs7 16 in
  let padded = padder plaintext in
  let blocks = Bytearray.split_every_n padded 16 in

  let temp_block = ref iv in
  List.map ~f:(fun cur_block ->
      (* keep track of the last encrypted block *)
      let crypted = encrypt_single_block_cbc key !temp_block cur_block in
      temp_block := crypted;
      crypted
    ) blocks
  |> Array.concat
