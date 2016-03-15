val encryption_oracle : int array -> (int array * string)
(** takes plaintext, returns ciphertext encrypted with
    (randomly chosen) ECB or CBC, and with 5-10 bytes
    of randomly chosen prefix and suffix appended to plaintext **)
