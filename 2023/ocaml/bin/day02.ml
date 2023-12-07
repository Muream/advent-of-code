open Core

let red_pat = Re.Perl.compile_pat {|(\d+) red|}
let green_pat = Re.Perl.compile_pat {|(\d+) green|}
let blue_pat = Re.Perl.compile_pat {|(\d+) blue|}
let max_list lst = List.fold lst ~init:0 ~f:max

let max_of_match matches =
  List.map matches ~f:(fun m -> Re.Group.get m 1 |> int_of_string) |> max_list
;;

let part1 lines =
  List.mapi lines ~f:(fun i line ->
    let max_red = Re.all red_pat line |> max_of_match in
    let max_green = Re.all green_pat line |> max_of_match in
    let max_blue = Re.all blue_pat line |> max_of_match in
    if max_red > 12 || max_green > 13 || max_blue > 14 then 0 else i + 1)
  |> List.fold ~init:0 ~f:( + )
;;

let part2 lines =
  List.map lines ~f:(fun line ->
    let max_red = Re.all red_pat line |> max_of_match in
    let max_green = Re.all green_pat line |> max_of_match in
    let max_blue = Re.all blue_pat line |> max_of_match in
    max_red * max_green * max_blue)
  |> List.fold ~init:0 ~f:( + )
;;

let () =
  let part1_res_test = part1 (In_channel.read_lines "../input/day02-test.txt") in
  Printf.printf "Day 02 - Part 01 - test: %i\n" part1_res_test
;;

let () =
  let part1_res = part1 (In_channel.read_lines "../input/day02.txt") in
  Printf.printf "Day 02 - Part 01: %i\n" part1_res
;;

let () =
  let part2_res_test = part2 (In_channel.read_lines "../input/day02-test.txt") in
  Printf.printf "Day 02 - Part 02 - test: %i\n" part2_res_test
;;

let () =
  let part2_res = part2 (In_channel.read_lines "../input/day02.txt") in
  Printf.printf "Day 02 - Part 02: %i\n" part2_res
;;
