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

let () =
  let part1_res_test = part1 (In_channel.read_lines "../input/day01-01-test.txt") in
  Printf.printf "Day 01 - Part 01 - test: %i\n" part1_res_test;
  let part1_res = part1 (In_channel.read_lines "../input/day01-01.txt") in
  Printf.printf "Day 01 - Part 01: %i\n" part1_res
;;
