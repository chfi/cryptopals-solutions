val ecb_or_cbc_oracle : ?key:(int array option) -> int array -> int array
(** takes plaintext, returns ciphertext encrypted with
    (randomly chosen) ECB or CBC, and with 5-10 bytes
    of randomly chosen prefix and suffix appended to plaintext **)

val ecb_encrypted : int array -> bool
(** returns true if it is possible that the given ciphertext
    was encrypted using ecb; that is, if there were any repeats of blocks *)
