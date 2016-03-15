val int_array_of_ascii_string : string -> int array

val int_array_of_hex_string : string -> int array

val int_array_of_base64_string : string -> int array


val ascii_string_of_int_array : int array -> string

val hex_string_of_int_array : int array -> string

val base64_string_of_int_array : int array -> string


val pad_int_array_pkcs7 : int -> int array -> int array

val unpad_int_array_pkcs7 : int -> int array -> int array

val split_every_n : int array -> int -> int array list
