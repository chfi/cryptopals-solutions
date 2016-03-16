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

(* First we need a function to find the first byte of the unknown string. *)
let generate_byte_dictionary block_size prefix =
  let bytes = List.init 255 ~f:(fun i -> i) in
  (* this list contains each input plaintext.
     we can then use bytes as the value in, for example, an alist. *)
  let plaintexts = List.map bytes
      ~f:(fun b -> Array.append prefix [|b|])
  in
  let ciphertexts = List.map plaintexts encrypter in
  (* retrieve the first block from the generated ciphertexts *)
  let blocks = List.map ciphertexts
      ~f:(fun ar -> Array.slice ar 0 16)
  in
  List.zip_exn blocks bytes

(* this could & should be done in a much nicer manner, i think.
   especially more functionally.

  like, create a function to decrypt an arbitrary byte in a text,
   given an appropriate prefix, then fold or map over the ciphertext...
   or something like that.
*)
let rec crack decrypted ciphertext_size =
  let dec_len = Array.length decrypted in
  print_endline ("DECRYPTING BYTE #" ^ (string_of_int dec_len));
  let str = Bytearray.ascii_string_of_int_array decrypted in
  print_endline ("DECRYPTED: " ^ str);
  print_endline "";
  let block_size = 16 in
  if dec_len >= ciphertext_size
  then
    []
  else
    let block_examined = dec_len / block_size in
    (* now we must generate the plaintext, such that there exists
       one block with 15 known bytes and 1 unknown byte. *)
    let prefix =
      if dec_len < block_size
      then
        Array.append
          (* 65 is ASCII 'A' *)
        (Array.init (block_size - dec_len - 1) ~f:(const 65))
        decrypted
      else
        (* else we want to look at the last 15 bytes *)
        Array.slice decrypted (-15) 0
    in
    (* the ciphertext block that we generate a dictionary of *)
    let dict = generate_byte_dictionary block_size prefix in

    (* print_endline ("Prefix: " ^ (Bytearray.ascii_string_of_int_array prefix)); *)

    (* dictionary in hand, we generate the ciphertext.
       we need to create the prefix such that it is of the correct length;
       that is, so the block we're looking at consists of 15 known bytes
       and 1 unknown byte. *)
    let plaintext =
        (Array.init (block_size - (dec_len mod block_size) - 1) ~f:(const 65))
    in

    let ciphertext = encrypter plaintext in
    (* we pick out the block in question, and find the corresponding
       entry in the dictionary *)
    print_endline ("  BLOCK #" ^ (string_of_int block_examined));
    print_endline ("  START BYTE: " ^ (string_of_int (block_size * block_examined)));
    print_endline ("  END BYTE: " ^
                   (string_of_int ((block_size * block_examined) + block_size)));
    let cipherblock = Array.slice ciphertext
        (block_size * block_examined) ((block_size * block_examined) + block_size)
    in

    let byte = List.Assoc.find dict cipherblock in
    (* we assume that if the byte didn't decrypt, there's nothing more to decrypt.
       this is a silly assumption; a better endcase should be found. *)
    let dec = match byte with
      | Some b -> Array.append decrypted [|b|]
      | None   -> decrypted
    in

    (* If the latest byte didn't decrypt to anything, we're probably done...
       TODO: Handle this better! this is way too imperative *)
    if (dec = decrypted) then
      crack dec (ciphertext_size - 1)
    else
      crack dec ciphertext_size


let () =
  let blocksize = find_block_size in
  print_endline ("Blocksize: " ^ (string_of_int blocksize));

  let ecb_mode = uses_ecb in
  print_endline ("Uses ECB: " ^ (string_of_bool ecb_mode));

  let ciphertext_size = Array.length (encrypter [||]) in
  print_endline ("Ciphertext length: " ^ (string_of_int ciphertext_size));

  let decrypted = crack [||] ciphertext_size in
  let str = String.of_char_list
      (List.map decrypted ~f:(fun i -> char_of_int i))
  in
  print_endline str
