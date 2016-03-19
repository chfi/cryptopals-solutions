open Core.Std

type profile = { email : string;
                 uid : int;
                 role : string; }

let get_profile email =
  let stripped =
    String.rev (String.of_char_list
         (String.fold email ~init:[] ~f:(fun acc c ->
        match c with
        | '&' -> acc
        | '=' -> acc
        | c -> c :: acc )))
  in
  { email = stripped; uid = 10; role = "user" }

let encode_profile { email; uid; role } =
  "email=" ^ email ^ "&uid=" ^ (string_of_int uid) ^ "&role=" ^ role

let key = Array.init 16 ~f:(fun _ -> Random.int 256)

let encrypt_profile email =
  let p = get_profile email in
  let s = encode_profile p in
  Aes.encrypt_ecb (Bytearray.int_array_of_ascii_string s) key

let profile_for email =
  encode_profile (get_profile email)

(* TODO: lots of this is hardcoded, like the length of the prepad
   when getting the admin block. should fix that. also the code is
   pretty ugly, should be factored out and cleaned up

   works tho! *)

let () =
  (* 'admin' padded so that it could be the last block, assuming
     pkcs#7 padding *)
  let admin_pt = "admin" ^
                 (Bytearray.ascii_string_of_int_array
                    (Array.init 11 ~f:(const 11)))

  in

  (* First, try out various lengths of emails to find one we can use;
     we want to have one long enough so that we have at least one whole
     block under our control. *)
  let email = "@baar.com" in
  let lengths = List.init (64-(String.length email)) ~f:(fun i -> i) in
  let emails = List.map lengths ~f:(fun l ->
      (String.init l ~f:(const 'A')) ^ email)
  in

  let encrypted_profs = List.map emails ~f:encrypt_profile in
  (* now, we want to find the length of the input that corresponds
     to at least one block filled with A *)

  let repeats = List.map encrypted_profs
      ~f:(fun e -> Frequencyanalysis.count_block_repeats e 16)
  in

  (* lets pick out the one with two repeating blocks, just because. *)
  let reps_with_lens = List.zip_exn repeats lengths in
  let (len,_) = List.find_exn reps_with_lens ~f:(fun (_,r) -> r = 2) in

  let email =
    String.init 10 ~f:(const 'A') ^ admin_pt ^ (String.init len ~f:(const 'A')) in
  (* let email = admin_pt ^ (String.init len ~f:(const 'A')) in *)
  let encrypted = encrypt_profile email in

  let admin_block = Array.slice encrypted 16 32 in
  let dec_adm = Aes.decrypt_ecb admin_block key in
  print_endline (Bytearray.ascii_string_of_int_array dec_adm);

  (* now we need to encrypt a profile with the correct length,
     that is, long enough such that the final block contains "user", encrypted. *)
  (* we can do this by encrypting emails of various lengths, until we find
     the border, where the plaintext is a multiple of 16 in length, then adding
     4 to that length. *)
  let email = "abcdef@gh.com" in
  let encrypted = encrypt_profile email in

  let cracked =
    Array.append
      (Array.slice encrypted 0 (-16))
      admin_block
  in
  let decrypted = Aes.decrypt_ecb cracked key in
  print_endline (Bytearray.ascii_string_of_int_array decrypted)
