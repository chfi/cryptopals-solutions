let in1 = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
let out1 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

let base64table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let byte_of_hex str =
  int_of_string ("0x" ^ str);;

(* build up the bytes sequence by looking at the hex string, step by step *)
let bytes_of_hex str =
  (* first create the bytes sequence, assume even number of hex chars *)
  let len = (String.length str) / 2 in
  (* helper function picks out a string consisting of two chars from the given index,
     from the bytes_of_hex argument *)
  let pair i = (String.sub str (2*i) 2) in
  Bytes.init len ((fun x -> Char.chr (byte_of_hex (pair x))));;

let ints_of_bytes bs =
  Array.init (Bytes.length bs) (fun x -> Char.code (Bytes.get bs x))

let ints_of_hex hs =
  ints_of_bytes (bytes_of_hex hs)

(* let hex_of_bytes bs = *)
(*   let len = (String.length str) * 2 in *)
  (* most resp. least significant nybble *)
  (* let msn x = (Bytes.get x)  *)

  (* String.init len (fun x ->  *)

let byte_triple_to_base64_quad (x,y,z) =
  let a = (0b11111100 land x) lsr 2 in
  let b = ((0b00000011 land x) lsl 4) lor ((0b11110000 land y) lsr 4) in
  let c = ((0b00001111 land y) lsl 2) lor ((0b11000000 land z) lsr 6) in
  let d = (0b00111111 land z) in
  (a,b,c,d);;


(* build list of 4-arrays, each being a base64-letter, then concat the list into a single
   long array *)
let base64_of_ints is =
  (* builds a triple containing three consecutive bytes from the input array *)
  let triple i ar = (Array.get ar i, Array.get ar (i+1), Array.get ar (i+2)) in
  (* converts a triple into a base64-encoded quad *)
  let base64_of_tri = byte_triple_to_base64_quad in

  let normalized =
    match (Array.length is) mod 3 with
    | 0 -> is
    | 1 -> Array.append is [|0;0|]
    | 2 -> Array.append is [|0|]
    | _ -> is
  in

  (* picks out the appropriate element of the tuple generated from the ith index *)
  let helper i offset ar =
    let (a,b,c,d) = base64_of_tri (triple i ar) in
    match offset with
    | 0 -> a
    | 1 -> b
    | 2 -> c
    | 3 -> d
    | _ -> raise (Failure ((string_of_int i) ^ " mod 4 not in [0..3] - impossible."))
  in

  (* let list = [] in *)
  (* let arr = Array.make len 0 in *)
  let iterhelper i ar =
    let temp = Array.make 4 0 in
    Array.set temp 0 (helper i 0 ar);
    Array.set temp 1 (helper i 1 ar);
    Array.set temp 2 (helper i 2 ar);
    Array.set temp 3 (helper i 3 ar);
    temp
  in
  let rec rechelper ar =
    match (Array.length ar) with
    | 0 -> []
    | 3 -> (iterhelper 0 ar) :: rechelper (Array.sub ar 3 ((Array.length ar) - 3))
    | n -> (iterhelper 0 ar) :: rechelper (Array.sub ar 3 ((Array.length ar) - 3))
    (* | n -> *)
  in
  Array.concat (rechelper normalized);;

let base64_to_string b = String.get base64table b

let base64_array_to_string =
  Array.map base64_to_string

let show_base64 ar padding =
  let padhelper i =
    match i with
    | 0 -> ""
    | 1 -> "="
    | 2 -> "=="
    | _ -> ""
  in
  let b64 = Array.map base64_to_string ar in
  let str = String.init (Array.length b64) (fun i -> Array.get b64 i) in
  str ^ (padhelper padding);;

let chal1 =
  let bs = ints_of_bytes (bytes_of_hex in1) in
  let pad = (Array.length bs) mod 3 in
  let out = show_base64 (base64_of_ints bs) pad in
  if String.compare out out1 == 0 then true else false



let in2a = "1c0111001f010100061a024b53535009181c"
let in2b = "686974207468652062756c6c277320657965"
let out2 = "746865206b696420646f6e277420706c6179"



(** challenge 2 **)

(* should check to see lengths of ar1, ar2 are equal *)
let xor ar1 ar2 =
  let len = Array.length ar1 in
  let result = Array.init len (fun i -> ((Array.get ar1 i) lxor (Array.get ar2 i))) in
  result;;


(* let chal2 = *)
(*   let i1 = ints_of_hex in2a in *)
(*   let i2 = ints_of_hex in2b in *)
(*   let o = ints_of_hex out2 in *)
(*   let res = xor i1 i2 in *)



(** challenge 3 **)

let freqtable =
  let tbl = Hashtbl.create 64 in
  Hashtbl.add tbl ' ' 0.00189;
  Hashtbl.add tbl '.' 0.03167;
  Hashtbl.add tbl 'a' 0.08167;
  Hashtbl.add tbl 'b' 0.01492;
  Hashtbl.add tbl 'c' 0.02782;
  Hashtbl.add tbl 'd' 0.04253;
  Hashtbl.add tbl 'e' 0.12702;
  Hashtbl.add tbl 'f' 0.02228;
  Hashtbl.add tbl 'g' 0.02015;
  Hashtbl.add tbl 'h' 0.06094;
  Hashtbl.add tbl 'i' 0.06966;
  Hashtbl.add tbl 'j' 0.00153;
  Hashtbl.add tbl 'k' 0.00772;
  Hashtbl.add tbl 'l' 0.04025;
  Hashtbl.add tbl 'm' 0.02406;
  Hashtbl.add tbl 'n' 0.06749;
  Hashtbl.add tbl 'o' 0.07507;
  Hashtbl.add tbl 'p' 0.01929;
  Hashtbl.add tbl 'q' 0.00095;
  Hashtbl.add tbl 'r' 0.05987;
  Hashtbl.add tbl 's' 0.06327;
  Hashtbl.add tbl 't' 0.09056;
  Hashtbl.add tbl 'u' 0.02758;
  Hashtbl.add tbl 'v' 0.00978;
  Hashtbl.add tbl 'w' 0.02360;
  Hashtbl.add tbl 'x' 0.00150;
  Hashtbl.add tbl 'y' 0.01974;
  Hashtbl.add tbl 'z' 0.00074;
  (* reduce score for control chars; if there are many, the score will sink like a rock *)
  (* for i=0 to 31 do *)
  (*   Hashtbl.add tbl (Char.chr i) (0.0) *)
  (* done; *)
  (* Hashtbl.add tbl (Char.chr 127) (0.0); *)
  tbl;;

let getcharfreq c =
  let norm = Char.lowercase c in
  if Hashtbl.mem freqtable norm then Hashtbl.find freqtable norm else 0.0;;
  (* Hashtbl.find freqtable norm;; *)


let in3 = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
let in3b = ints_of_hex in3
(* the key is (decimal) 88 *)

let singlexor ar key =
  Array.map (fun x -> x lxor key) ar

let string_of_ints is =
  let len = Array.length is in
  String.init len (fun x -> Char.chr (Array.get is x))

(* returns the count of the char c in the string str *)
let count_letter str c =
  let rec helper s i =
    let tail = if (String.length s) == 0 then ""
          else String.sub s 1 (String.length s - 1) in
    match (String.length s) with
    | 0 -> 0
    | n -> (if (String.get s 0) == c then 1 else 0) + helper tail i
  in
  helper str 0;;

(* chi^2 test of expected frequency of letters *)
let score_string_chi2 str =
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  let count_table = Hashtbl.create 64 in

  (* first, calculate the count of each letter in the string. *)
  String.iter (fun c -> Hashtbl.add count_table c (count_letter str c)) alphabet;
  let len = String.length str in

  (* then, calculate the expected count of each letter in a string of that length. *)
  let expected_table = Hashtbl.create 64 in
  String.iter (fun c -> Hashtbl.add expected_table c
                  (getcharfreq c *. (float_of_int len))) alphabet;
                  (* (int_of_float (getcharfreq c *. (float_of_int len)))) alphabet; *)

  (* then take the sum of the square of the differences over the expected count,
     for each letter in the alphabet. *)
  let helper i =
    let c = String.get alphabet i in
    let expected = (Hashtbl.find expected_table c) in
    let count = float_of_int (Hashtbl.find count_table c) in
    ((count -. expected) ** 2.0) /. expected
  in
  let sqdifs = Array.init 26 helper in
  let score = Array.fold_left (fun x y -> x +. y) 0.0 sqdifs in
  score;;

let singledecrypt ar key =
  let xored = singlexor ar key in
  let str = string_of_ints xored in
  let score = score_string_chi2 str in
  (str, score, key);;


let try_all_bytes_decrypt ar =
  let attempts = Array.init 255 (fun i -> singledecrypt ar i) in
  Array.sort (fun (_,x,_) (_,y,_) -> compare x y) attempts;
  attempts;;


(** challenge 4 **)

(* basically, map try_all_bytes_decrypt over each read string,
   then find the decrypted string with the highest score. i guess.*)

let file4 = "4.txt"
let in4 =
  let arr = Array.create 60 "" in
  let ic = open_in file4 in
  try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    Array.set arr 0
    print_endline line;          (* write the result to stdout *)
    flush stdout;                (* write on the underlying device now *)
    close_in ic                  (* close the input channel *) 

  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)
