open Core.Std

let ecb_or_cbc ciphertext =
  if (Frequencyanalysis.count_block_repeats ciphertext 16) > 1
  then "ECB"
  else "CBC"

let () =
  Random.self_init ();
  let plaintext = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let plain_bytes = Bytearray.int_array_of_ascii_string plaintext in

  let encrypted = List.init 10 ~f:(fun i -> Oracle.ecb_or_cbc_oracle plain_bytes) in
  let mode = List.map encrypted ~f:(fun e -> ecb_or_cbc e) in
  List.iter mode ~f:(fun m -> print_endline ("Guessed - " ^ m))
