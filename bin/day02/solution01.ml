let parse_line line = line |> String.split_on_char ' ' |> List.map int_of_string

let rec differences lst =
  match lst with
  | x1 :: x2 :: rest -> (x2 - x1) :: differences (x2 :: rest)
  | _ -> []

let is_safe line =
  let diff = differences line in
  List.for_all (fun x -> x <= 3 && x >= 1) diff
  || List.for_all (fun x -> x >= -3 && x <= -1) diff

let () =
  let filename = "./bin/day02/test.txt" in
  let lines = Advent_of_ocaml_2024.Utils.read_file filename in
  let res =
    List.map parse_line lines |> List.map is_safe
    |> List.filter (fun x -> x)
    |> List.length
  in
  print_int res
