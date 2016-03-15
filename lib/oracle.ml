open Core.Std

let random_bytes len =
  Array.init len ~f:(fun _ -> Random.int 256)

let encryption_oracle plaintext =
  (* Random.int 256 *)
  let module C = Cryptokit.Cipher in

  let key = random_bytes 16 in
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
    (Aes.encrypt_cbc padded key iv, "CBC")
  else
    (Aes.encrypt_ecb padded key, "ECB")


