open Core.Std


let key = [|192; 157; 86; 25; 215; 44; 40; 236; 17; 226; 209; 69; 205; 118; 159; 92|]


let encrypter plaintext =
  let unknown =
    Bytearray.int_array_of_base64_string
      "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
  in
  Aes.encrypt_ecb (Array.append plaintext unknown) key
  (* Oracle.encryption_oracle ~key:(Some key) (Array.append plaintext unknown) *)

(** Find the block size by feeding various length inputs and looking at the
    output lengths **)
let find_block_size =
  let input = List.init 32 ~f:(fun i -> (Array.init i (const (int_of_char 'A')))) in
  let blocks = List.map input ~f:(fun b -> encrypter b) in
  let lengths =
    List.map blocks ~f:(fun b -> Array.length b)
  in
  let diff_lens = List.dedup lengths in
  (* this is bad and ugly tho *)
  if (List.length diff_lens) > 1
  then
    (List.nth_exn diff_lens 1) - (List.hd_exn diff_lens)
  else
    0


let uses_ecb =
  let input = List.init 64 ~f:(fun i -> (Array.init i (const (int_of_char 'A')))) in
  let blocks = List.map input ~f:(fun b -> encrypter b) in
  List.exists blocks ~f:(fun b -> Oracle.ecb_encrypted b)

(* i need to think about how this works. *)
let find_byte_encryption block_size byte =
  let plaintext =
    (* 65 is A *)
    Array.append (Array.init (block_size - 1) ~f:(const 65)) ([|byte|])
  in
  let encrypted = encrypter plaintext in
  encrypted.(block_size)

(* so, for each byte 'b' in the unknown text,
   i want to generate a plaintext of the length so that 'b' is the
   byte byte in the following my plaintext, then I want to append
   each possible byte to that, and create a lookup table.
   lookup table in hand, i then put the original generated plaintext
   into the oracle, pop out the encrypted block in question - the one
   that's shared by my plaintext and the unknown plaintext.
   i then look up that block in the table i've generated, and woop,
   got the first byte!

   change the plaintext correspondingly ("shifting" it along, maybe?)
   and repeat until the entire plaintext is found.
*)

let () =
  let blocksize = find_block_size in
  print_endline ("Blocksize: " ^ (string_of_int blocksize));

  let ecb_mode = uses_ecb in
  print_endline ("Uses ECB: " ^ (string_of_bool ecb_mode))




