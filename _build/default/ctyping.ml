
open Cast
(* Open Tast *)

let check_file f = match f with
	| [] -> []
	| h::t -> [h]
;;