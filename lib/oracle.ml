open Core.Std

let random_bytes len =
  Array.init len ~f:(fun _ -> Random.int 256)

(* TODO: refactor this to be specifically
   an ecb/cbc oracle, in name *)
let ecb_or_cbc_oracle ?(key=(None)) plaintext =
  (* Random.int 256 *)
  let module C = Cryptokit.Cipher in
  let key =
    match key with
    | Some k -> k
    | None   -> random_bytes 16
  in

  (* let key = random_bytes 16 in *)
  let pre_len = (Random.int 6) + 5 in
  let suf_len = (Random.int 6) + 5 in

  let prefix = random_bytes pre_len in
  let suffix = random_bytes suf_len in

  let padded =
    (Array.append prefix
       (Array.append plaintext suffix))
  in

  let cbc = Random.bool () in
  if cbc
  then
    let iv = Array.init 16 ~f:(fun i -> 0) in
    (* let iv = random_bytes 16 in *)
    Aes.encrypt_cbc padded key iv
  else
    Aes.encrypt_ecb padded key

let ecb_encrypted ciphertext =
  if (Frequencyanalysis.count_block_repeats ciphertext 16) > 1
  then true
  else false
