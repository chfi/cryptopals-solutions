open Core.Std

let decryptfile fp =
  let file = In_channel.create fp in
  let lines =
    List.map (In_channel.input_lines file)
      ~f:String.strip
  in
  let byteslist = List.map lines ~f:Bytearray.int_array_of_hex_string in
  let attemptslist = List.map byteslist ~f:Xorcrypto.try_all_bytes_decrypt in
  let bestattemptslist =
      List.map attemptslist (fun x -> Array.get x 0)
  in
  bestattemptslist

let printdecrypted i (st,sc,k) =
  print_endline ("Index: " ^ (string_of_int i));
  print_endline ("String: " ^ st);
  print_endline ("Score: " ^ (string_of_float sc));
  print_endline ("Key: " ^ (string_of_int k))

let () =
  let filepath = "4.txt" in
  let results = decryptfile filepath in
  List.iteri results ~f:printdecrypted
