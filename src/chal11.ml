open Core.Std

let ecb_or_cbc ciphertext =
  (* Frequencyanalysis.count_block_repeats ciphertext 16 *)
  if (Frequencyanalysis.count_block_repeats ciphertext 16) > 1
  then "ECB"
  else "CBC"

let () =
  Random.self_init ();
  let plaintext = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let plain_bytes = Bytearray.int_array_of_ascii_string plaintext in

  let encrypted = List.init 10 ~f:(fun i -> Oracle.encryption_oracle plain_bytes) in
  let mode = List.map encrypted ~f:(fun (e,_) -> ecb_or_cbc e) in
  List.iter2_exn encrypted mode
    ~f:(fun (e,m) g -> print_endline ("Guessed - " ^ g ^ "; Actually - " ^ m))
