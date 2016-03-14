open Core.Std

(* Turns out this didn't work! *)
let find_hamming_distances fp =
  let file = In_channel.create fp in
  let lines = List.map (In_channel.input_lines file) ~f:String.strip in
  let bytes_list = List.map lines ~f:Bytearray.int_array_of_base64_string in
  let distances = List.map bytes_list
      ~f:(fun ar -> Frequencyanalysis.running_hamming_distance ar 16)
  in
  List.sort
    ~cmp:(fun (x,_) (y,_) -> compare x y)
    (List.zip_exn distances lines)

(* but this did. *)
let count_16_byte_block_repeats fp =
  let file = In_channel.create fp in
  let lines = List.map (In_channel.input_lines file) ~f:String.strip in
  let bytes_list = List.map lines ~f:Bytearray.int_array_of_base64_string in
  let repeats = List.map bytes_list
      ~f:(fun ar -> Frequencyanalysis.count_block_repeats ar 16)
  in
  List.sort
    ~cmp:(fun (x,_) (y,_) -> compare y x)
    (List.zip_exn repeats lines)

let () =
  let filename = "8.txt" in
  let repeats = count_16_byte_block_repeats filename in
  List.iter repeats ~f:(fun (d,l) ->
      print_endline ("# repeats - " ^ (string_of_int d));
      print_endline ("  " ^ l));
