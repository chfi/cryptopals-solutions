open Core.Std

(* maybe i'll have to rewrite this to use hashmaps or something instead of
   a profile type... *)

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

let profile_for email =
  encode_profile (get_profile email)



let key = [|192; 157; 86; 25; 215; 44; 40; 236; 17; 226; 209; 69; 205; 118; 159; 92|]

(* let key = Array.init 16 ~f:(fun _ -> Random.int 256) *)

(* really, i might as well just look at the encoded strings. that's what matters,
   anyway.

   no, this won't work. i didn't consider the fact that i can't enter & or =...
   one way to do it is to simply create a block containing only "admin",
   and filling the rest with padding. but can i do that? can i assume
   i know the padding being used?
   fuck it, probably.

   then i need to fill up the first block, which has 6 filled letters.
   that's just 11 bytes.


   with the "admin block" in hand,
   i need to fill the rest out so that it's exactly 16 or 32 (or another multiple)
   bytes long. I could do this by testing various inputs lengths. might as well,
   more fun to automate this.

   that is, generate a list containing the numbers 1->32, and for each number,
   add a byte to the input to profile_for, and map encrypt over it.
   then fold over the list, looking to find the index at which point the
   encrypted size changes; that's the number of bytes we want the input to be.
   then just swap in the admin block for the last block and decrypt it.
*)

let () =
  let encoded = profile_for "foo@bar.com" in
  let enc_bytes = Bytearray.int_array_of_ascii_string encoded in
  let encrypted = Aes.encrypt_ecb enc_bytes key in
  let decrypted = Aes.decrypt_ecb encrypted key in

  let first_block = Array.init 11 ~f:(fun _ -> 65) in
  let admin_block =
    let admin = Bytearray.int_array_of_ascii_string "admin" in
    let padding = Array.init 11 ~f:(fun _ -> 11) in
    Array.append admin padding
  in

  let pt = Array.append first_block admin_block in

  let enc_crack = Aes.encrypt_ecb enc_bytes key in
  let admin_enc = Array.slice enc_crack 16 32 in


  print_endline "hm"
