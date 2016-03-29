val score_string_char : string -> float
(** Scores a string according to letter frequency. A string containing
    many characters that are more frequent in the English language
    is scored higher than one with few frequent characters. **)

val running_hamming_distance : int array -> int -> float
(** [running_hamming_distance ar n] calculates the normalized Hamming
    distance for each neighboring pair of [n]-long blocks in [ar]. **)

val find_key_size : int array -> int -> (int * float) list
(** [find_key_size ar max_keysize] returns a list of keysize * distance
    pairs, where the distance is the running Hamming distance for
    that keysize. Sorted so that lowest distance is first; lower
    distance is likely to be actual XOR keysize. **)

val count_block_repeats : int array -> int -> int
(** [count_block_repeats ar n] counts the maximum number of times
    any [n]-long block in [ar] is found in [ar].

    E.g [count_block_repeats [|1;2;1;2;3;1;2|] 2] returns [2], since
    the block [|1;2|] is repeated twice; the last 2 is cut off. **)

