open Printf

exception Panic of string
let panic s = raise (Panic s)


module Par : sig

  type 'a par

  val return : 'a -> 'a par
  val (>>=) : 'a par -> ('a -> 'b par) -> 'b par
  val sat : (char -> bool) -> char par

  val parse : 'a par -> string -> 'a

end = struct

  type _ par =
    | Return : 'a -> 'a par
    | Bind : 'a par * ('a -> 'b par) -> 'b par
    | Sat : (char -> bool) -> char par

  let return x = Return x
  let (>>=) par f = Bind (par,f)
  let sat pred = Sat pred

  let parse : 'a par -> string -> 'a =
    fun par0 the_input_string ->
    let rec loop : type a. int -> a par -> (int -> a -> 'r) -> 'r = fun n par k ->
      match par with
      | Return x -> k n x
      | Sat pred ->
         let c = the_input_string.[n] in
         if pred c then k (n+1) c else panic (sprintf "Sat/no:%c" c)
      | Bind (p,f) ->
         loop n p (fun n a ->
             let p2 = f a in
             loop n p2 k)
    in
    let k0 _n a = a in
    loop 0 par0 k0

end


module Ast = struct
  type exp =
    | Num of int
    | Add of exp * exp
    | Sub of exp * exp
    | Mul of exp * exp
end

module Parser : sig
  val parse_exp : string -> Ast.exp
end = struct

  open Par

  let digit_of_char : char -> int =
    let code0 = Char.code '0' in
    fun c -> Char.code c - code0

  let digit : int par =
    sat (fun c -> c >= '0' && c <= '9') >>= fun c ->
    return (digit_of_char c)

  let int2 : int par =
    digit >>= fun d1 ->
    digit >>= fun d2 ->
    return (10*d1+d2)

  let exp =
    int2 >>= fun n ->
    return (Ast.Num n)

  let parse_exp = Par.parse exp

end




module Interpreter : sig
  type value = int
  val eval : Ast.exp -> value
end = struct
  type value = int
  open Ast
  let rec eval = function
    | Num n -> n
    | Add (e1,e2) -> eval e1 + eval e2
    | Sub (e1,e2) -> eval e1 - eval e2
    | Mul (e1,e2) -> eval e1 * eval e2
end

module PrettyPrint : sig
  val pp_exp : Ast.exp -> string
  val pp_value : Interpreter.value -> string
end = struct
  open Ast
  let rec pp_exp = function
    | Num n -> sprintf "%d" n
    | Add (e1,e2) -> sprintf "(%s + %s)" (pp_exp e1) (pp_exp e2)
    | Sub (e1,e2) -> sprintf "(%s - %s)" (pp_exp e1) (pp_exp e2)
    | Mul (e1,e2) -> sprintf "(%s * %s)" (pp_exp e1) (pp_exp e2)
  let pp_value n = sprintf "%d" n
end

let main() =
  printf "*ofun*\n";
  let _e0 = Ast.(Sub (Num 100, Mul (Add (Num 20, Num 9), Num 2))) in

  let s = "100 - (20+9) * 2" in
  printf "s: %s\n" s;

  let open Parser in
  let _e1() = parse_exp s in

  let e = parse_exp "43x" in

  let open PrettyPrint in
  printf "e: %s\n" (pp_exp e);
  let open Interpreter in
  let v = eval e in
  printf "v: %s\n" (pp_value v)
