open Printf

exception Panic of string
let panic s = raise (Panic s)

module Ast = struct
  type exp =
    | Num of int
    | Add of exp * exp
    | Sub of exp * exp
    | Mul of exp * exp
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

module Parser : sig
  val parse : string -> Ast.exp
end = struct
  let parse _ = panic "parse"
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
  let s = "100 - (20+9) * 2" in
  printf "s: %s\n" s;
  (*let _e = Parser.(parse s) in*)
  let e = Ast.(Sub (Num 100, Mul (Add (Num 20, Num 9), Num 2))) in
  let open PrettyPrint in
  printf "e: %s\n" (pp_exp e);
  let open Interpreter in
  let v = eval e in
  printf "v: %s\n" (pp_value v)
