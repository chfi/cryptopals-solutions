open Core.Std

(* byte-wise XOR on two byte arrays *)
let xor =
  Array.map2_exn ~f:(fun x y -> x lxor y)


let xor_single_byte ar key =
  Array.map (fun x -> x lxor key) ar


let decrypt_single_byte_xor ar key =
  let xored = xor_single_byte ar key in
  let str = Bytearray.ascii_string_of_int_array xored in
  let score = Frequencyanalysis.score_string_char str in
  (str, score, key)


let repeating_key_xor ints key =
  (* XOR the int at index i with the byte at key index (i mod key length) *)
  let xori ~i ~byte ~k =
    let keylen = Array.length k in
    byte lxor k.(i mod keylen)
  in
  Array.mapi ~f:(fun i c -> xori ~i:i ~byte:c ~k:key) ints


let repeating_key_xor_string str skey =
  let is = Bytearray.int_array_of_ascii_string str in
  let key = Bytearray.int_array_of_ascii_string skey in
  let cipheris = repeating_key_xor is key in
  Bytearray.ascii_string_of_int_array cipheris


let get_every_nth arr n offset =
  List.range ~stride:n offset (Array.length arr)
  |> List.map ~f:(fun i -> arr.(i))
  |> Array.of_list


let get_blocks arr n =
  List.range 0 n
  |> List.map ~f:(get_every_nth arr n)


let try_all_bytes_decrypt ar =
  Array.init ~f:(fun i -> decrypt_single_byte_xor ar i) 255
  |> Array.sorted_copy ~cmp:(fun (_,x,_) (_,y,_) -> compare y x)


let try_decrypt_repeating_xor ar keysize =
  get_blocks ar keysize
  (* 'blocks' contains in its first position the first byte of each keysize-long
     block in ar, the second bytes in the second position, etc. *)
  |> List.map ~f:(fun arr -> (try_all_bytes_decrypt arr).(0))


let find_repeating_xor_key ar keysize =
  List.map ~f:(fun (_,_,k) -> k) (try_decrypt_repeating_xor ar keysize)
  |> Array.of_list
