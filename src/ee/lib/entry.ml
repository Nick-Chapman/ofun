open Printf

module ED = Effect.Deep

exception Panic of string
let panic s = raise (Panic s)

type _ Effect.t += Conversion_failure : string -> int Effect.t

let convert str =
  try int_of_string str with
  | Failure _ -> Effect.perform (Conversion_failure str)

let sum_ints_loop acc =
  let rec loop () =
    let str = input_line stdin in
    let i = convert str in
    acc := !acc + i;
    printf "sum_ints, got %d%, acc=%d\n%!" i (!acc);
    loop()
  in loop()

let sum_ints () =
  let acc = ref 0 in
  ED.match_with sum_ints_loop acc {
      retc = (fun _ -> panic "unexpected return from sum_ints.loop");
      exnc = (function
              | End_of_file -> !acc
              | exn -> raise exn
             );
      effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Conversion_failure str ->
           printf "handling conversion failure: %s\n%!" str;
           Some (fun (k:(c,_) ED.continuation) -> ED.continue k 1000)
        | _ -> None)
    }

let example1() =
  printf "example1: sum_ints...\n%!";
  let res = sum_ints() in
  printf "Sum = %d\n%!" res
