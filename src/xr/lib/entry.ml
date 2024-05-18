open Printf

module R = Routes

let root () = R.nil
let users () = R.(s "users" / s "get" /? nil)
let sum () = R.(s "sum" / int / int /? nil)
let get_user () = R.(s "user" / str / int64 /? nil)
let r1 = R.( sum () @--> fun x y -> (x+y) )

let show () =
  printf "1:%s\n%!" (R.string_of_path (root()));
  printf "2:%s\n%!" (R.string_of_path (users()));
  printf "3:%s\n%!" (R.string_of_path (sum()));
  printf "4:%s\n%!" (R.string_of_path (get_user()));
  printf "5:%s\n%!" (R.string_of_route r1);
  ()

type com =
   | SetV : string * int -> com
   | GetV : string -> com

let r_set = R.( s "setv" / str / int /? nil @--> fun x v -> SetV (x,v) )
let r_get = R.( s "getv" / str /? nil @--> fun x -> GetV x )

let r : com R.router =
  R.one_of
    [ r_set
    ; r_get
    ]


let main() =
  printf "explore routes...\n%!";
  show()
