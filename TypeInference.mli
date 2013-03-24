open Lambda;;

val typ_inference: expr -> typ;;
exception TypeError of string;; 

