
type 'a par

val return : 'a -> 'a par
val (>>=) : 'a par -> ('a -> 'b par) -> 'b par
val fail : 'a par
val alts : 'a par list -> 'a par
val sat : (char -> bool) -> char par

exception Error of string

val parse : 'a par -> string -> 'a
