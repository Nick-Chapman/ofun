
(* 2024 reimplementation of 4-value parsers. In Ocaml. Using GADTTs *)

open Printf

exception Error of string
let error s = raise (Error s)

type _ par =
  | Ret : 'a -> 'a par
  | Fail : 'a par
  | Sat : (char -> bool) -> char par
  | Bind : 'a par * ('a -> 'b par) -> 'b par
  | Alt : 'a par * 'a par -> 'a par

let return x = Ret x
let (>>=) par f = Bind (par,f)
let fail = Fail
let alts xs = List.fold_left (fun p q -> Alt(p,q)) Fail xs
let sat pred = Sat pred

type 'a res = Good of int * 'a | Bad of int

type ('a,'b) k4 =
  { succ : int -> 'a -> 'b res
  ; eps : 'a -> 'b res
  ; fail : unit -> 'b res
  ; err : int -> 'b res
  }

let parse : 'a par -> string -> 'a =
  fun the_parser the_input_string ->
  let z = String.length the_input_string in
  let conclude : 'a res -> 'a = function
    | Bad n -> error (sprintf "parse error at %d" n)
    | Good (n,a) -> if n < z then error (sprintf "unconsumed from %d" n) else a
  in
  let rec run : type a. int -> a par -> (a,'r) k4 -> 'r res =
    fun n par ({succ;eps;fail;err} as k) ->
    match par with
    | Ret x -> eps x
    | Fail -> fail ()
    | Sat pred ->
       if n >= z then fail() else
         let c = the_input_string.[n] in
         if pred c then succ (n+1) c else fail()
    | Alt (p1,p2) ->
       run n p1 { succ
                ; eps = (fun a1 -> run n p2 { succ
                                            ; eps = (fun _ -> eps a1) (*left-biased*)
                                            ; fail = (fun () -> eps a1)
                                            ; err })
                ; fail = (fun () -> run n p2 k)
                ; err }
    | Bind (p,f) ->
       run n p { succ = (fun n a -> run n (f a) { succ
                                                ; eps = (succ n) (*consume*)
                                                ; fail = (fun () -> err n) (*fail->error*)
                                                ; err })
               ; eps = (fun a -> run n (f a) k)
               ; fail
               ; err }
  in
  let k4_final =
    { succ = (fun n a -> Good (n,a))
    ; eps = (fun a -> Good (0,a))
    ; fail = (fun () -> Bad 0)
    ; err = (fun n -> Bad n)
    } in
  conclude (run 0 the_parser k4_final)
