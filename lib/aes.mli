val decrypt_ecb : int array -> int array -> int array
(** [decrypt_ecb ciphertext key] decrypts [ciphertext] using AES-128
    with chaining mode ECB and key [key], pads using PKCS#7 **)

val encrypt_ecb : int array -> int array -> int array
(** [encrypt_ecb plaintext key] encrypts [plaintext] using AES-128
    with chaining mode ECB and key [key], pads using PKCS#7 **)

val decrypt_cbc : int array -> int array -> int array -> int array
(** [decrypt_cbc ciphertext key iv] decrypts [ciphertext] using AES-128
    with chaining mode CBC, IV [iv], and key [key], pads using PKCS#7 **)

val encrypt_cbc : int array -> int array -> int array -> int array
(** [encrypt_cbc plaintext key iv] encrypts [plaintext] using AES-128
    with chaining mode CBC, IV [iv], and key [key], pads using PKCS#7 **)
