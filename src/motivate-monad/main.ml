open Printf

(*...
let get_number () : int =
  Random.int 100

let add_numbers a b : int =
  a + b

(* initial example *)
let _example () : int =
  let a = get_number() in
  let b = get_number() in
  let ab = add_numbers a b in
  ab

(* add prints *)
let example () : int =
  printf "example..\n";
  let a = get_number() in
  printf "example..got a: %d\n" a;
  let b = get_number() in
  printf "example..got b: %d\n" b;
  let ab = add_numbers a b in
  printf "example..added ->: %d\n" ab;
  ab

let _example_runner () : unit =
  let res = example () in
  printf "example: everything coolio -- %d\n" res


(* introduce potential failure when getting a number *)
let get_number () : int option =
  let res = Random.int 100 in
  if res mod 5 = 0 then None else Some res

let _example () : int option =
  printf "example..\n";
  match get_number() with
  | None -> None
  | Some a ->
     printf "example..got a: %d\n" a;
     match get_number() with
     | None -> None
     | Some b ->
        printf "example..got b: %d\n" b;
        let ab = add_numbers a b in
        printf "example..added ->: %d\n" ab;
        Some ab


(* rewrite option-match using bind *)
(*let bind = Option.bind*)
let bind opt f = match opt with
  | None -> None
  | Some x -> f x

let _example () : int option =
  printf "example..\n";
  bind (get_number()) (fun a ->
      printf "example..got a: %d\n" a;
      bind (get_number()) (fun b ->
          printf "example..got b: %d\n" b;
          let ab = add_numbers a b in
          printf "example..added ->: %d\n" ab;
          Some ab))

(* rewrite option-match using infix >>= *)
let ( >>= ) = bind
let _example () : int option =
  printf "example..\n";
  get_number() >>= fun a ->
  printf "example..got a: %d\n" a;
  get_number() >>= fun b ->
  printf "example..got b: %d\n" b;
  let ab = add_numbers a b in
  printf "example..added ->: %d\n" ab;
  Some ab

(* rewrite option-match using let* *)
let ( let* ) = bind
let _example () : int option =
  printf "example..\n";
  let* a = get_number() in
  printf "example..got a: %d\n" a;
  let* b = get_number() in
  printf "example..got b: %d\n" b;
  let ab = add_numbers a b in
  printf "example..added ->: %d\n" ab;
  Some ab


(* add may now also fail *)
let add_numbers a b : int option =
  if a > b then Some (a+b) else None

let example () : int option =
  printf "example..\n";
  let* a = get_number() in
  printf "example..got a: %d\n" a;
  let* b = get_number() in
  printf "example..got b: %d\n" b;
  let* ab = add_numbers a b in (* change here *)
  printf "example..added ->: %d\n" ab;
  Some ab

let _example_runner () =
  match example () with
  | None -> printf "example: something failed!\n"
  | Some res -> printf "example: everything coolio -- %d\n" res

...*)

(* change to a new monad *)
type 'a or_error = Good of 'a | Bad of string

let ( let* ) oe f = match oe with
  | Bad m -> Bad m
  | Good x -> f x

let get_number tag : int or_error =
  let res = Random.int 100 in
  if res mod 5 = 0 then Bad ("get number failed for: "^tag) else Good res

let add_numbers a b : int or_error =
  if a > b then Good (a+b) else Bad "add_numbers failed"

let example () : int or_error =
  let* a = get_number "a" in
  let* b = get_number "b" in
  add_numbers a b

let example_runner () =
  match example () with
  | Bad m -> printf "example: something failed! -- %s\n" m
  | Good res -> printf "example: everything coolio -- %d\n" res

let go() =
  Random.self_init();
  print_endline "go..";
  let () = example_runner () in
  print_endline "done"



let () = go()
