open Core.Std

let base64table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

(* helper functions *)
(* converts a length 2 string assumed to represent a byte in hexadecimal form *)
let int_of_hex str = int_of_string ("0x" ^ str)


(* inverse of above; shows an int as a string *)
let hex_of_int i = Printf.sprintf "%02x" i


let byte_triple_to_base64_quad (x,y,z) =
  let a = (0b11111100 land x) lsr 2 in
  let b = ((0b00000011 land x) lsl 4) lor ((0b11110000 land y) lsr 4) in
  let c = ((0b00001111 land y) lsl 2) lor ((0b11000000 land z) lsr 6) in
  let d = (0b00111111 land z) in
  (a,b,c,d)


let base64_quad_to_byte_triple (a,b,c,d) =
  let x = ((0b00111111 land a) lsl 2) lor ((0b00110000 land b) lsr 4) in
  let y = ((b land 0b00001111) lsl 4) lor ((c land 0b00111100) lsr 2) in
  let z = ((c land 0b00000011) lsl 6) lor ((d land 0b00111111)) in
  (x,y,z)


(* int array-level helper functions *)
(* returns padding size as second in pair *)
let base64_ints_of_byte_ints bs =
  let triple_to_quad_array x y z =
    let (a,b,c,d) = byte_triple_to_base64_quad (x,y,z) in
    [|a;b;c;d|]
  in

  (* append zeroes padding if necessary *)
  let (normalized, padlength) =
    match (Array.length bs) mod 3 with
    | 1 -> (Array.append bs [|0;0|], 1)
    | 2 -> (Array.append bs [|0|], 1)
    | 0 | _ -> (bs, 0)
  in

  let rec helper ar =
    match Array.length ar with
    | 0 -> []
    | n -> (triple_to_quad_array
             ar.(0) ar.(1) ar.(2) :: helper (Array.slice ar 3 0))
  in
  (Array.concat (helper normalized), padlength)


let byte_ints_of_base64_ints b64 =
  (* create list of 3-byte arrays, then concat them *)
  let quad_to_triple_array a b c d =
    let (x,y,z) = base64_quad_to_byte_triple (a,b,c,d) in
    [|x;y;z|]
  in

  let rec helper ar =
    match Array.length ar with
    | 0 -> []
    | n -> (quad_to_triple_array
             ar.(0) ar.(1) ar.(2) ar.(3)) :: helper (Array.slice ar 4 0)
  in
  Array.concat (helper b64)


(* functions to convert from text strings to int arrays;
   all these arrays represent regular old bytes. *)
let int_array_of_ascii_string str =
  Array.init ~f:(fun i -> Char.to_int (String.get str i)) (String.length str)


let int_array_of_hex_string str =
  if (String.length str) mod 2 <> 0
    then raise (Invalid_argument "Hex strings must be of even length");
  let byte i = int_of_hex (String.sub str (2*i) 2) in
  Array.init ~f:(fun i -> byte i) ((String.length str) / 2)


let int_array_of_base64_string str =
  (* first, convert the base64 string to an array of ints, each
     int representing one base64 "byte" *)
  (* count equals-signs to find the padding *)
  let padding = String.count (String.slice str 0 0) ~f:(fun c -> c = '=') in
  (* replace the padding with 0s in the int array *)
  let normalized = String.slice str 0 (- padding) in
  let pad_array = Array.init ~f:(fun _ -> 0) padding in
  let base64_ints = Array.init
      ~f:(fun i -> String.index_exn base64table (normalized.[i]))
      (String.length normalized)
  in

  let b64 = Array.append base64_ints pad_array in

  (* create list of 3-byte arrays, then concat them *)
  let quad_to_triple_array a b c d =
    let (x,y,z) = base64_quad_to_byte_triple (a,b,c,d) in
    [|x;y;z|]
  in

  let rec helper ar =
    match Array.length ar with
    | 0 -> []
    | n -> (quad_to_triple_array
             ar.(0) ar.(1) ar.(2) ar.(3)) :: helper (Array.slice ar 4 0)

  in
  let result = Array.concat (helper b64) in
  Array.slice result 0 (- padding)


(** functions to convert from int arrays to various types of text strings.
   the int arrays all represent regular bytes **)
let ascii_string_of_int_array ar =
  let char_list = List.map (Array.to_list ar) ~f:(fun i -> Char.of_int_exn i) in
  String.of_char_list char_list

let hex_string_of_int_array ar =
  let hex_list = List.init (Array.length ar) ~f:(fun i -> hex_of_int ar.(i)) in
  String.concat hex_list

let base64_string_of_int_array ar =
  let (b64_ints,pad) = base64_ints_of_byte_ints ar in
  let str = String.init
      ~f:(fun i -> String.get base64table (b64_ints.(i)))
      ((Array.length b64_ints) - pad)
  in
  let pad = String.init ~f:(fun _ -> '=') pad in
  str ^ pad

(* padding functions *)
let pad_int_array_pkcs7 blocklen ar =
  let padlen = if ((Array.length ar) mod blocklen) = 0
    then 0
    else blocklen - ((Array.length ar) mod blocklen) in
  let padding = Array.init ~f:(fun i -> padlen) padlen in
  Array.append ar padding


let unpad_int_array_pkcs7 ar =
  let padlen = Array.nget ar (-1) in
  (* see that the array is properly padded, raise exception if not *)
  let padding = Array.slice ar (-padlen) 0 in
  let correct = Array.init ~f:(fun i -> padlen) padlen in
  if padding <> correct then raise (Invalid_argument
                                      "Array not pkcs7 padded");
  Array.slice ar 0 (-padlen)


(* returns a list where each element is an n-long array from 'ar',
   potentially skipping the last few elements if the array length
   is not divisible by n. *)
let split_every_n ar n =
  let extra_length = (Array.length ar) mod n in
  let chopped = Array.slice ar 0 (- extra_length) in
  let elem_len = (Array.length chopped) / n in
  List.init ~f:(fun i -> Array.slice chopped (i*n) (((i+1)*n))) elem_len
