open Printf

exception Error of string
let error s = raise (Error s)
let panic s = error (sprintf "panic:%s" s)

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

  open Par4
  let (let*) = (>>=)

  let digit_of_char : char -> int =
    let code0 = Char.code '0' in
    fun c -> Char.code c - code0

  let digit : int par =
    let* c = sat (fun c -> c >= '0' && c <= '9') in
    return (digit_of_char c)

  let _int2 : int par =
    let* d1 = digit in
    let* d2 = digit in
    return (10*d1+d2)

  let int : int par =
    let rec loop acc =
      alts [ return acc ; let* d = digit in loop (10*acc+d) ]
    in let* d = digit in loop d

  let exp =
    let* n = int in
    return (Ast.Num n)

  let parse_exp = Par4.parse exp

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
  printf "*ofun*(parser tests)\n";
  let cases =
    [ "43"
    ; "432"
    ; "x"
    ; "4"
    ; ""
    ; "4321"
    ; "43x21"
    ] in
  let test s =
    printf "test: '%s' -> " s;
    let res = (try PrettyPrint.pp_exp (Parser.parse_exp s) with Par4.Error s -> s) in
    printf "%s\n" res
  in
  List.iter test cases

let _main() =
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
