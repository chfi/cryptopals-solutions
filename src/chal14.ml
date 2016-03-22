open Core.Std

(* is the string random but constant? then this should be pretty easy;
   could just clean up the code from the last challenge and combine it
   with that from chal 12.

   i'll assume it's constant; it makes more sense for that to be the case. *)

let target_base64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

let prefix =
  Array.init (Random.int 64) ~f:(fun _ -> (Random.int 256))

let key =
  Array.init 16 ~f:(fun _ -> Random.int 256)

let oracle plaintext =
  let target_plaintext = Bytearray.int_array_of_base64_string target_base64 in
  let pt = Array.concat [prefix; plaintext; target_plaintext] in
  Aes.encrypt_ecb pt key


let () =
  (* first find the prefix+suffix length; this is done by making a note of
     how long the ciphertext is when the oracle is fed the empty string,
     then counting how large our plaintext needs to be for the ciphertext
     to change in size. *)
  let base_len = Array.length (oracle [||]) in
  let block_i = List.init 16 ~f:(fun i -> i) in
  (* generate plaintexts and corresponding ciphertexts *)
  let pt_blocks = List.map block_i ~f:(fun i -> Array.init i ~f:(const 65)) in
  let cts = List.map pt_blocks ~f:oracle in
  (* pick out their lengths and save them *)
  let pt_lens = List.map pt_blocks ~f:(fun a -> Array.length a) in
  let ct_lens = List.map cts ~f:(fun a -> Array.length a) in
  (* zip them together so we can keep track of their relation *)
  let pt_ct_lens = List.zip_exn pt_lens ct_lens in
  (* pick the first one whose ciphertext length is not the same as that of the
     empty plaintext *)
  (* let (pt_len,_) = List.find_exn ~f:(_,cl) -> cl <> base_len in *)
  let (pt_len,_) = List.hd_exn
      (List.drop_while pt_ct_lens
         ~f:(fun (_,cl) -> cl = base_len)) in

  let prefix_suffix_len = base_len - (pt_len - 1) in

  (* find the length of the prefix by adding generating a plaintext block,
     appending it to itself, then prefixing one byte at a time until we
     get a (n extra) number of repeated blocks. *)
  let prefix_len = 0 in

  (* with the prefix length in hand, we also know the suffix length. *)
  let suffix_len = prefix_suffix_len - prefix_len in

  (* and we also know where our plaintext is inserted, so we know which
     block to look at - now we can simply solve this as we did challenge 12. *)

