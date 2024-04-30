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

  let lit : char -> unit par = fun x ->
    let* _ = sat (fun c -> c=x) in
    return ()

  let white = alts [lit ' '; lit '\t']

  let nibble : 'a par -> 'a par = fun par ->
    let* x = par in
    let rec loop () = alts [return x; let* () = white in loop()] in
    loop ()

  let key c = nibble (lit c)

  let digits : int par =
    let rec loop acc =
      alts [ return acc ; let* d = digit in loop (10*acc+d) ]
    in let* d = digit in loop d

  let int = nibble digits

  let exp =
    let rec exp() =
      let atom =
        alts [ (let* n = int in return (Ast.Num n))
             ; (let* () = key '(' in
                let* e = exp() in
                let* () = key ')' in
                return e)
          ]
      in
      (* TODO: correct precedence! *)
      let rec loop e1 =
        alts [ return e1
             ; (let* () = key '+' in let* e2 = atom in loop (Ast.Add (e1,e2)))
             ; (let* () = key '-' in let* e2 = atom in loop (Ast.Sub (e1,e2)))
             ; (let* () = key '*' in let* e2 = atom in loop (Ast.Mul (e1,e2)))
          ]
      in
      let* e0 = atom in
      loop e0
    in
    exp()

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

let try_parse_and_pp : string -> string = fun s ->
  try PrettyPrint.pp_exp (Parser.parse_exp s) with Par4.Error s -> s

let _test() =
  printf "*ofun*(parser tests)\n";
  let cases =
    [ "43"
    ; "432"
    ; "x"
    ; "4"
    ; ""
    ; "4321"
    ; "43x21"
    ; "100-(20+9)*2"
    ; "100 - (20+9) * 2"
    ]
  in
  let test s =
    printf "test: '%s' -> " s;
    let res = try_parse_and_pp s in
    printf "%s\n" res
  in
  List.iter test cases

let main() =
  printf "*ofun*\n";
  (*test();*)
  let _e0 = Ast.(Sub (Num 100, Mul (Add (Num 20, Num 9), Num 2))) in

  let s = "100 - (20+9) * 2" in
  printf "s: %s\n" s;

  let e = Parser.parse_exp s in

  printf "e: %s\n" (PrettyPrint.pp_exp e); (* wrong precedence! *)

  let v = Interpreter.eval e in
  printf "v: %s\n" (PrettyPrint.pp_value v) (* wrong, shoud be 42 *)
