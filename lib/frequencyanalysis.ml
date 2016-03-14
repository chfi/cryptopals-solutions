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
  let norm = Char.lowercase c in
  match List.Assoc.find freqalist norm with
  | Some x -> x
  | None -> 0.0

let count_non_alphanumerics str =
  let chars =
    List.init 10 ~f:(fun i -> char_of_int (48 + i)) @
    List.init 26 ~f:(fun i -> char_of_int (65 + i)) @
    List.init 26 ~f:(fun i -> char_of_int (97 + i)) @
    ['.';'\'';',';'\"';' ';]
  in
  String.count str ~f:(fun c -> not (List.mem chars c))

(* returns the count of the char c in the string str,
   if ?cs:Some true, then case sensitive, otherwise insensitive *)
let count_letter str c =
  let norm = String.lowercase str in
  let rec helper s i =
    let tail = if (String.length s) = 0 then ""
          else String.sub s 1 (String.length s - 1) in
    match (String.length s) with
    | 0 -> 0
    | n -> (if (String.get s 0) = c then 1 else 0) + helper tail i
  in
  helper norm 0

(* chi^2 test of expected frequency of letters,
   plus a penalty for non alphanumerics; not used! *)
let score_string_chi2 str =
  let norm = String.strip str in
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  (* first, calculate the count of each letter in the string. *)
  let count_alist =
    List.map (String.to_list alphabet)
             (fun c -> c, float_of_int (count_letter norm c))
  in
  let len = String.length norm in

  (* then, calculate the expected count of each letter in a string of that length. *)
  let expected_alist =
    List.map (String.to_list alphabet)
             (fun c -> c, getcharfreq c *. (float_of_int len))
  in

  (* then take the sum of the square of the differences over the expected count,
     for each letter in the alphabet. *)
  let helper i =
    let c = String.get alphabet i in
    let expected = List.Assoc.find_exn expected_alist c in
    let count = List.Assoc.find_exn count_alist c in
    ((count -. expected) ** 2.0) /. expected
  in

  let sqdifs = Array.init 26 helper in
  let nonalphacount = count_non_alphanumerics norm in
  let calculated = Array.fold sqdifs ~init:0.0 ~f:(fun x y -> x +. y) in

  calculated +. (float_of_int (nonalphacount * 100))

let score_string_char str =
  let normalized = String.lowercase str in
  String.fold ~init:0.0 ~f:(fun acc c -> acc +. (getcharfreq c)) normalized

(* bitwise hamming distance between int arrays,
   exception thrown if arrays are of different lengths *)
let hamming_distance ar1 ar2 =
  if (Array.length ar1) <> (Array.length ar2)
    then raise (Invalid_argument "Arrays must be of equal length");
  let single_dist i1 i2 =
    (* looks at the 8 rightmost bits, since
       we're treating the ints as eytes *)
    let diff = i1 lxor i2 in
    let list = [0;1;2;3;4;5;6;7] in
    List.count list ~f:(fun i -> if 1 land (diff lsr i) = 1 then true else false)
  in
  Array.fold (Array.map2_exn ar1 ar2 single_dist) ~init:0 ~f:(+)

(* hamming distance normalized by division with array length *)
let normalized_hamming_distance ar1 ar2 =
  if (Array.length ar1) <> (Array.length ar2)
    then raise (Invalid_argument "Arrays must be of equal length");
  let dist = hamming_distance ar1 ar2 in
  (float_of_int dist) /. (float_of_int (Array.length ar1))

(* returns a list where each element is an n-long array from 'ar',
   potentially skipping the last few elements if the array length
   is not divisible by n. *)
let split_every_n ar n =
  let extra_length = (Array.length ar) mod n in
  let chopped = Array.slice ar 0 ((Array.length ar) - extra_length) in
  let elem_len = (Array.length chopped) / n in
  List.init elem_len ~f:(fun i -> Array.slice chopped (i*n) (((i+1)*n)))

let running_hamming_distance ar len =
  (* remove the last few parts of the arrays, so that they can be evenly
     split into 'len' sized blocks *)
  let blocks = split_every_n ar len in
  let indices = List.range 1 (List.length blocks) in
  let sum = List.fold indices ~init:0.0
      ~f:(fun acc i -> acc +. (normalized_hamming_distance
                                 (List.nth_exn blocks (i-1))
                                 (List.nth_exn blocks i)))
  in
  let num_blocks = List.length indices in
  sum /. (float_of_int num_blocks)

let find_key_size ints maxsize =
  if maxsize > (Array.length ints) / 4 then raise (Failure "Too large keysize");
  let keysizes = List.range 2 maxsize in
  let comparebytes keysize =
    running_hamming_distance ints keysize
  in
  let dists = List.map keysizes ~f:(fun ks -> (ks,(comparebytes ks))) in
  List.sort ~cmp:(fun (_,x) (_,y) -> compare x y) dists


let count_single_block_occurences block ar =
  let blocks = split_every_n ar (Array.length block) in
  List.count blocks ~f:(fun b -> b = block)


(* splits ar int len-size blocks, then counts the number of repeats
   of each block. *)
let count_block_repeats ar len =
  let blocks = split_every_n ar len in
  let hashtbl = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  (* add each block to the hashtable *)
  List.iter blocks ~f:(fun b ->
      Hashtbl.set hashtbl
        ~key:b
        ~data:(List.count blocks ~f:(fun b' -> b = b')));
  List.fold (Hashtbl.data hashtbl) ~init:0 ~f:(fun acc i -> Int.max acc i)
  (* List.max_elt (Hashtbl.data hashtbl) ~cmp:(fun a b -> Int.max a b) *)
  (* Hashtbl.iter_vals hashtbl ~f:(fun v -> v *)

