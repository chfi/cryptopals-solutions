open Core.Std

let target_base64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

let prefix =
  Array.init (Random.int 64) ~f:(fun _ -> (Random.int 256))

let key =
  Array.init 16 ~f:(fun _ -> Random.int 256)

let oracle plaintext =
  let target_plaintext = Bytearray.int_array_of_base64_string target_base64 in
  let pt = Array.concat [prefix; plaintext; target_plaintext] in
  Aes.encrypt_ecb pt key


let longest_repeat arr block_size =
  let blocks = Bytearray.split_every_n arr block_size in

  (* counts how many times the head appears successively, i.e.
     [a;a;b;c] -> (a,2)
     [b;a;a;d] -> (b,1) *)
  let rec hd_succ_count' (p,r) l =
    let continue = (p = List.hd_exn l) in
    match List.length l with
    | 1 -> if continue then (p,r+1) else (p,r)
    | n -> if continue then hd_succ_count' (p,r+1) (List.tl_exn l)
                       else (p,r)
  in
  let hd_succ_count l =
    hd_succ_count' (List.hd_exn l, 0) l in

  (* (kind of) the tail of each element *)
  let tails =
    let len = List.length blocks in
    List.init (len-1)
      ~f:(fun i ->
          let tail = List.slice blocks i (len-i) in
          tail)
  in
  let hd_counts = List.map tails ~f:hd_succ_count in
  hd_counts


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
     get a (n extra) number of repeated blocks.

     ... is it this simple? what if there's a repeated block in the prefix
     or suffix; there likely isn't, but there could be.

     could just make a huge known plaintext - longer than the prefix + suffix.
     let's not, for now.

     let's have a 3-block long plaintext, and look for successive repeated blocks.
  *)

  let known_pt_block = Array.init 16 ~f:(const 65) in
  let known_pt = Array.concat [known_pt_block; known_pt_block; known_pt_block] in
  let prefixes = List.init 16 ~f:(fun i -> Array.init i ~f:(const 1)) in
  let pts = List.mapi prefixes ~f:(fun i p -> (i+16,Array.append p known_pt)) in
  let cts = List.map pts ~f:(fun (l,pt) -> (l, oracle pt)) in
  let repeats = List.map cts ~f:(fun (l,ct) -> (l,longest_repeat ct 16)) in
  (* finds (what is probably) the ciphertext version of our known plaintext block;
     it's extremely unlikely that the prefix would contain 48 consecutive identical
     bytes *)
  (* picks out the most-repeated block from each ciphertext *)
  (* this is wrong; need to keep the length somehow *)
  let sorted = List.map repeats ~f:(fun (len,list) -> List.hd_exn (List.sort list
                                       ~cmp:(fun (_,x) (_,y) -> compare x y)))
  in

  (* the first one with three repeats is what we're looking for *)
  (* this contains the ciphertext block that was repeated three times *)
  let (x,_) = List.find_exn sorted ~f:(fun (_,r) -> r = 3) in

  (* then we need to find the length of the plaintext that led to that
     block existing in the ciphertext *)

  let prefix_len = 0 in

  (* the index of the first whole block that's in our control *)
  let our_block_number =

  (* with the prefix length in hand, we also know the suffix length. *)
  let suffix_len = prefix_suffix_len - prefix_len in

  (* and we also know where our plaintext is inserted, so we know which
     block to look at - now we can simply solve this as we did challenge 12. *)
  ()

