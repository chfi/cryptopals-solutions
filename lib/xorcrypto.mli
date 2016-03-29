val xor : int array -> int array -> int array
(** Byte-wise XOR on two byte arrays. Throws exception if
    arrays are of different sizes. **)

val repeating_key_xor : int array -> int array -> int array
(** [repeating_key_xor ar key] returns the int array produced by
    XORing [key] with each [length key]-size block of [ar]. **)

val repeating_key_xor_string : string -> string -> string
(** [repeating_key_xor_string str key] returns the string produced
    by XORing the bytes of [key] with each [length key]-size block
    of the bytes of [str]. **)

val try_all_bytes_decrypt : int array -> (string * float * int) array
    (** Decrypts a given array that's been encrypted using XOR with a
        single-byte key. Returns an array sorted so that the first entry
        is most likely to be the correctly decrypted one; the tuple
        is of the form [(plaintext * score * key)]. **)

val try_decrypt_repeating_xor : int array -> int -> (string * float * int) list
(** [try_decrypt_repeating_xor ar keysize] attempts to decrypt an array
    that's been XORed against a key of size [keysize]. The list returned
    is of the form [(plaintext * score * key)], and sorted by score. **)

val find_repeating_xor_key : int array -> int -> int array
(** [find_repeating_xor_key ar keysize] returns the array that was
    used to decrypt [ar] using repeating key XOR. **)
