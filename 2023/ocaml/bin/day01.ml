open Core

let part1 lines =
  let numbers =
    List.map lines ~f:(fun line ->
      let chars = String.to_list line in
      let numbers = List.filter chars ~f:Char.is_digit in
      let number =
        String.of_char (List.hd_exn numbers) ^ String.of_char (List.last_exn numbers)
      in
      Int.of_string number)
  in
  let total = List.reduce_exn numbers ~f:( + ) in
  total
;;

(** Return whatever number this string starts with *)
let get_number string =
  match string with
  | s when String.is_prefix s ~prefix:"one" -> Some "1"
  | s when String.is_prefix s ~prefix:"two" -> Some "2"
  | s when String.is_prefix s ~prefix:"three" -> Some "3"
  | s when String.is_prefix s ~prefix:"four" -> Some "4"
  | s when String.is_prefix s ~prefix:"five" -> Some "5"
  | s when String.is_prefix s ~prefix:"six" -> Some "6"
  | s when String.is_prefix s ~prefix:"seven" -> Some "7"
  | s when String.is_prefix s ~prefix:"eight" -> Some "8"
  | s when String.is_prefix s ~prefix:"nine" -> Some "9"
  | s when String.is_prefix s ~prefix:"1" -> Some "1"
  | s when String.is_prefix s ~prefix:"2" -> Some "2"
  | s when String.is_prefix s ~prefix:"3" -> Some "3"
  | s when String.is_prefix s ~prefix:"4" -> Some "4"
  | s when String.is_prefix s ~prefix:"5" -> Some "5"
  | s when String.is_prefix s ~prefix:"6" -> Some "6"
  | s when String.is_prefix s ~prefix:"7" -> Some "7"
  | s when String.is_prefix s ~prefix:"8" -> Some "8"
  | s when String.is_prefix s ~prefix:"9" -> Some "9"
  | _ -> None
;;

(** Lists all the digits in the string whether they're spelled out of written as a numbers *)
let get_numbers string =
  let rec aux string acc =
    match string with
    | "" -> acc
    | str ->
      (match get_number str with
       | Some n -> aux (String.sub str ~pos:1 ~len:(String.length str - 1)) (n :: acc)
       | None -> aux (String.sub str ~pos:1 ~len:(String.length str - 1)) acc)
  in
  List.rev (aux string [])
;;

let part2 lines =
  let numbers =
    List.map lines ~f:(fun line ->
      let numbers = get_numbers line in
      let number = List.hd_exn numbers ^ List.last_exn numbers in
      Int.of_string number)
  in
  let total = List.reduce_exn numbers ~f:( + ) in
  total
;;

let () =
  let part1_res_test = part1 (In_channel.read_lines "../input/day01-01-test.txt") in
  Printf.printf "Day 01 - Part 01 - test: %i\n" part1_res_test
;;

let () =
  let part1_res = part1 (In_channel.read_lines "../input/day01-01.txt") in
  Printf.printf "Day 01 - Part 01: %i\n" part1_res
;;

let () =
  let part2_res_test = part2 (In_channel.read_lines "../input/day01-02-test.txt") in
  Printf.printf "Day 01 - Part 02 - test: %i\n" part2_res_test
;;

let () =
  let part2_res = part2 (In_channel.read_lines "../input/day01-02.txt") in
  Printf.printf "Day 01 - Part 02: %i\n" part2_res
;;
