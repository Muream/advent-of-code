open Core

let pattern = Re2.create_exn "(one|two|three|four|five|six|seven|eight|nine)"

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

let numberify string =
  Re2.rewrite_exn pattern ~template:"_\\1_" string
  |> String.substr_replace_all ~pattern:"_one_" ~with_:"1"
  |> String.substr_replace_all ~pattern:"_two_" ~with_:"2"
  |> String.substr_replace_all ~pattern:"_three_" ~with_:"3"
  |> String.substr_replace_all ~pattern:"_four_" ~with_:"4"
  |> String.substr_replace_all ~pattern:"_five_" ~with_:"5"
  |> String.substr_replace_all ~pattern:"_six_" ~with_:"6"
  |> String.substr_replace_all ~pattern:"_seven_" ~with_:"7"
  |> String.substr_replace_all ~pattern:"_eight_" ~with_:"8"
  |> String.substr_replace_all ~pattern:"_nine_" ~with_:"9"
;;

let part2 lines =
  let numbers =
    List.map lines ~f:(fun line ->
      print_endline line;
      let numberified = numberify line in
      print_endline numberified;
      let chars = String.to_list numberified in
      let numbers = List.filter chars ~f:Char.is_digit in
      let number =
        String.of_char (List.hd_exn numbers) ^ String.of_char (List.last_exn numbers)
      in
      print_endline number;
      print_endline "";
      Int.of_string number)
  in
  let total = List.reduce_exn numbers ~f:( + ) in
  total
;;

let () =
  let part1_res_test = part1 (In_channel.read_lines "../input/day01-01-test.txt") in
  Printf.printf "Day 01 - Part 01 - test: %i\n" part1_res_test;
  let part1_res = part1 (In_channel.read_lines "../input/day01-01.txt") in
  Printf.printf "Day 01 - Part 01: %i\n" part1_res;
  let part2_res_test = part2 (In_channel.read_lines "../input/day01-02-test.txt") in
  Printf.printf "Day 01 - Part 02 - test: %i\n" part2_res_test;
  let part2_res = part2 (In_channel.read_lines "../input/day01-02.txt") in
  Printf.printf "Day 01 - Part 02: %i\n" part2_res
;;
