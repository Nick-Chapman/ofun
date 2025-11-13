
let _split =
  let rec loop acc i xs =
    if i == 0 then (List.rev acc,xs) else
      match xs with
      | [] -> (List.rev acc,[])
      | x::xs -> loop (x::acc) (i-1) xs
  in loop []

let split n xs =
  let open ExtLib.List in
  try split_nth n xs with Invalid_index _ -> (xs,[])

let _chop_list (n:int) : 'a list -> 'a list list =
  if n<1 then failwith "chop_list";
  let rec loop acc = function
    | [] -> List.rev acc
    | xs ->
       let (ys,zs) = split n xs in
       loop (ys::acc) zs
  in loop []

let _chop_list = BatList.ntake

let chop_list = CCList.chunks

let go() =
  let xs = [1;2;3;4;5;6;7;8;9] in
  let see xs = String.concat "." (List.map string_of_int xs) in
  let see2 ys = String.concat "|" (List.map see ys) in
  print_endline (see xs);
  print_endline (see2 (chop_list 1 xs));
  print_endline (see2 (chop_list 2 xs));
  print_endline (see2 (chop_list 3 xs));
  print_endline (see2 (chop_list 4 xs));
  print_endline (see2 (chop_list 5 xs));
  print_endline (see2 (chop_list 6 xs));
  print_endline (see2 (chop_list 7 xs));
  print_endline (see2 (chop_list 8 xs));
  print_endline (see2 (chop_list 9 xs));
  print_endline (see2 (chop_list 10 xs));
  print_endline (see2 (chop_list 11 xs));
  ()

let () = go()
