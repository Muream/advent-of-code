open Core
module IntSet = Set.Make (Int)

let number_pat = Re.Perl.compile_pat {|\d+|}

let get_numbers str =
  Re.all number_pat str
  |> List.map ~f:(fun m -> Re.Group.get m 0 |> int_of_string)
  |> IntSet.of_list
;;

let rec asdf list acc =
  match list with
  | [] | [ _ ] -> acc
  | _ :: t -> asdf t (acc * 2)
;;

let part1 lines =
  List.map lines ~f:(fun line ->
    let split = String.split_on_chars ~on:[ ':'; '|' ] line in
    let winning_nums = List.nth_exn split 1 |> get_numbers in
    let our_nums = List.last_exn split |> get_numbers in
    let won_nums = Set.inter winning_nums our_nums in
    if Set.length won_nums = 0 then 0 else asdf (Set.to_list won_nums) 1)
  |> List.reduce_exn ~f:( + )
;;

(* let part2 _lines = 0 *)

let () =
  let part1_res_test = part1 (In_channel.read_lines "../input/day04-01-test.txt") in
  Printf.printf "Day 04 - Part 01 - test: %i\n" part1_res_test
;;

let () =
  let part1_res = part1 (In_channel.read_lines "../input/day04.txt") in
  Printf.printf "Day 02 - Part 01: %i\n" part1_res
;;

(* let () = *)
(*   let part2_res_test = part2 (In_channel.read_lines "../input/day04-02-test.txt") in *)
(*   Printf.printf "Day 04 - Part 02 - test: %i\n" part2_res_test *)
(* ;; *)

(* let () = *)
(*   let part2_res = part2 (In_channel.read_lines "../input/day02.txt") in *)
(*   Printf.printf "Day 02 - Part 02: %i\n" part2_res *)
(* ;; *)
