open Core

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let () =
  let lines = In_channel.read_lines "input/day01-01.txt" in
  let numbers =
    List.map lines ~f:(fun line ->
      let chars = String.to_list line in
      let numbers = List.filter chars ~f:is_digit in
      let number =
        String.of_char (List.hd_exn numbers) ^ String.of_char (List.last_exn numbers)
      in
      Int.of_string number)
  in
  let total = List.reduce_exn numbers ~f:( + ) in
  Printf.printf "Result: %i\n" total
;;
