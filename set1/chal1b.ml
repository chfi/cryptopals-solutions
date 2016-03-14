let in1 = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
let out1 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

let base64table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"


(*
   first, do a pairwise translation of hex into bytes

   second, transform the byte string into base64
   this is probably best done by a recursive function that simply
   uses the Array.get or String.get functions to get three things out of the array at once.

   *)
