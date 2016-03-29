open Core.Std

let freqalist =
  ['a', 0.0651738;
   'b', 0.0124248;
   'c', 0.0217339;
   'd', 0.0349835;
   'e', 0.1041442;
   'f', 0.0197881;
   'g', 0.0158610;
   'h', 0.0492888;
   'i', 0.0558094;
   'j', 0.0009033;
   'k', 0.0050529;
   'l', 0.0331490;
   'm', 0.0202124;
   'n', 0.0564513;
   'o', 0.0596302;
   'p', 0.0137645;
   'q', 0.0008606;
   'r', 0.0497563;
   's', 0.0515760;
   't', 0.0729357;
   'u', 0.0225134;
   'v', 0.0082903;
   'w', 0.0171272;
   'x', 0.0013692;
   'y', 0.0145984;
   'z', 0.0007836;
   ' ', 0.1918182
  ]


let getcharfreq c =
  List.Assoc.find freqalist (Char.lowercase c)
  |> Option.value ~default:0.0


let count_non_alphanumerics str =
  let chars =
    List.init ~f:(fun i -> char_of_int (48 + i)) 10 @
    List.init ~f:(fun i -> char_of_int (65 + i)) 26 @
    List.init ~f:(fun i -> char_of_int (97 + i)) 26 @
    ['.';'\'';',';'\"';' ';]
  in
  String.count ~f:(fun c -> not (List.mem chars c)) str


(* returns the count of the char c in the string str,
   if ?cs:Some true, then case sensitive, otherwise insensitive *)
let count_letter str c =
  let norm = String.lowercase str in
  String.count ~f:((=) c) norm

(* chi^2 test of expected frequency of letters,
   plus a penalty for non alphanumerics; not used! *)
let score_string_chi2 str =
  let norm = String.strip str in
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  (* first, calculate the count of each letter in the string. *)
  let count_alist =
    List.map
      ~f:(fun c -> (c, float_of_int (count_letter norm c)) )
      (String.to_list alphabet)
  in
  let len = String.length norm in

  (* then, calculate the expected count of each letter in a string of that length. *)
  let expected_alist =
    List.map
      ~f:(fun c -> (c, getcharfreq c *. (float_of_int len)) )
      (String.to_list alphabet)
  in

  (* then take the sum of the square of the differences over the expected count,
     for each letter in the alphabet. *)
  let helper i =
    let c = alphabet.[i] in
    let expected = List.Assoc.find_exn expected_alist c in
    let count = List.Assoc.find_exn count_alist c in
    ((count -. expected) ** 2.0) /. expected
  in

  let sqdifs = Array.init ~f:helper 26 in
  let nonalphacount = count_non_alphanumerics norm in
  let calculated = Array.fold ~init:0.0 ~f:(fun x y -> x +. y) sqdifs in

  calculated +. (float_of_int (nonalphacount * 100))

let score_string_char str =
  String.lowercase str
  |> String.fold ~init:0.0 ~f:(fun acc c -> acc +. (getcharfreq c))


let hamming_distance ar1 ar2 =
  if (Array.length ar1) <> (Array.length ar2)
    then raise (Invalid_argument "Arrays must be of equal length");
    (* helper to calculate the hamming distance between two bytes *)
  let single_dist i1 i2 =
    let diff = i1 lxor i2 in
    List.range 0 8 (* each bit must be checked *)
    |> List.count ~f:(fun i -> 1 land (diff lsr i) = 1)
  in
  Array.fold ~init:0 ~f:(+) (Array.map2_exn ar1 ar2 single_dist)


(* hamming distance normalized by division with array length *)
let normalized_hamming_distance ar1 ar2 =
  if (Array.length ar1) <> (Array.length ar2)
    then raise (Invalid_argument "Arrays must be of equal length");
  let dist = hamming_distance ar1 ar2 in
  (float_of_int dist) /. (float_of_int (Array.length ar1))


(* normalized hamming distance calculated for each neighboring pair of blocks *)
let running_hamming_distance ar len =
  let blocks = Bytearray.split_every_n ar len in
  let indices = List.range 1 (List.length blocks) in
  (* TODO: this function could really use some cleaning up, maybe with option *)
  let sum = List.fold ~init:0.0
      ~f:(fun acc i -> acc +. (normalized_hamming_distance
                                 (List.nth_exn blocks (i-1))
                                 (List.nth_exn blocks i)))
      indices
  in
  let num_blocks = List.length indices in
  sum /. (float_of_int num_blocks)


let find_key_size ints maxsize =
  if maxsize > (Array.length ints) / 4 then raise (Failure "Too large keysize");
  let keysizes = List.range 2 maxsize in
  let ham_helper = running_hamming_distance ints in
  List.map ~f:(fun ks -> (ks,(ham_helper ks))) keysizes
  |> List.sort ~cmp:(fun (_,x) (_,y) -> compare x y)


let count_single_block_occurences block ar =
  Bytearray.split_every_n ar (Array.length block)
  |> List.count ~f:(fun b -> b = block)


(* splits ar int len-size blocks, then counts the number of repeats
   of each block. *)
let count_block_repeats ar len =
  let blocks = Bytearray.split_every_n ar len in
  let hashtbl = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  (* add each block to the hashtable *)
  List.iter blocks ~f:(fun b ->
      Hashtbl.set ~key:b ~data:(List.count ~f:((=) b) blocks) hashtbl );

  List.fold ~init:0 ~f:(fun acc i -> Int.max acc i) (Hashtbl.data hashtbl)
