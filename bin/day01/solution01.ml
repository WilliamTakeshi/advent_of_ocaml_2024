let parse_line line =
  match
    String.split_on_char ' ' line
    |> List.filter (( <> ) "")
    |> List.map int_of_string
  with
  | [ x; y ] -> (x, y)
  | _ -> failwith "Invalid input format"

let split_pairs pairs =
  List.fold_right
    (fun (x, y) (left, right) -> (x :: left, y :: right))
    pairs ([], [])

let total_distance left right =
  let sorted_left = List.sort compare left in
  let sorted_right = List.sort compare right in
  List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 sorted_left sorted_right

let () =
  let filename = "./bin/day01/test.txt" in
  let lines = Advent_of_ocaml_2024.Utils.read_file filename in
  let pairs = List.map parse_line lines in
  let left, right = split_pairs pairs in
  let distance = total_distance left right in
  Printf.printf "Total distance: %d\n" distance
