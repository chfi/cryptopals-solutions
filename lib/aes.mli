val decrypt_ecb : int array -> int array -> int array
(** ciphertext key **)

val encrypt_ecb : int array -> int array -> int array
(** plaintext key **)

val decrypt_cbc : int array -> int array -> int array -> int array
(** ciphertext key iv **)

val encrypt_cbc : int array -> int array -> int array -> int array
(** plaintext key iv **)
