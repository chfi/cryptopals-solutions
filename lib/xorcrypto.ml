open Core.Std

let xor ar1 ar2 =
  let len = Array.length ar1 in
  let result = Array.init len (fun i -> ((Array.get ar1 i) lxor (Array.get ar2 i))) in
  result

let singlexor ar key =
  Array.map (fun x -> x lxor key) ar

let singledecrypt ar key =
  let xored = singlexor ar key in
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
  let indices = List.range ~stride:n offset (Array.length arr) in
  Array.of_list (List.map indices (fun i -> arr.(i)))

let get_blocks arr n =
  let key_indices = List.range 0 n in
  List.map key_indices ~f:(get_every_nth arr n)


let try_all_bytes_decrypt ar =
  let attempts = Array.init 255 ~f:(fun i -> singledecrypt ar i) in
  Array.sort attempts ~cmp:(fun (_,x,_) (_,y,_) -> compare y x);
  attempts

let try_decrypt_repeating_xor ar keysize =
  let blocks = get_blocks ar keysize in
  (* 'blocks' contains in its first position the first byte of each keysize-long
     block in ar, the second bytes in the second position, etc. *)
  List.map blocks ~f:(fun arr -> (try_all_bytes_decrypt arr).(0))

let find_repeating_xor_key ar keysize =
  let keylist = List.map
      (try_decrypt_repeating_xor ar keysize)
      ~f:(fun (_,_,k) -> k)
  in
  Array.of_list keylist
