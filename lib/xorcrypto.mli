val repeating_key_xor : int array -> int array -> int array

val repeating_key_xor_string : string -> string -> string

val try_all_bytes_decrypt : int array -> (string * float * int) array

val try_decrypt_repeating_xor : int array -> int -> (string * float * int) list

val find_repeating_xor_key : int array -> int -> int array
