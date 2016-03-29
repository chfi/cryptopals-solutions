val int_array_of_ascii_string : string -> int array
(** Converts an ASCII-encoded string into an int (byte) array **)

val int_array_of_hex_string : string -> int array
(** Converts a hexadecimal-encoded string into an int (byte) array **)

val int_array_of_base64_string : string -> int array
(** Converts a Base64-encoded string into an int (byte) array **)


val ascii_string_of_int_array : int array -> string
(** Converts an int (byte) array into an ASCII-encoded string **)

val hex_string_of_int_array : int array -> string
(** Converts an int (byte) array into a hexadecimal-encoded string **)

val base64_string_of_int_array : int array -> string
(** Converts an int (byte) array into a Base64-encoded string **)


val pad_int_array_pkcs7 : int -> int array -> int array
(** [pad_int_array_pkcs7 p ar] returns an array beginning with [ar]
    but padded so that it is a multiple of [p] in length, using PKCS#7. **)

val unpad_int_array_pkcs7 : int array -> int array
(** unpads the given array, assuming it is padded using PKCS#7.
    If the array is not PKCS#7 padded, an exception is thrown. **)

val split_every_n : int array -> int -> int array list
(** [split_every_n ar n] returns a list containing each successive
    [n]-long subarray of [ar], e.g. [split_every_n [|1;2;3;4;5|] 2]
    returns [[|1;2|];[|3;4|]] **)
