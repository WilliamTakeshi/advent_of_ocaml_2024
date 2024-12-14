let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let parse_line line = line |> String.split_on_char ' ' |> List.map int_of_string

let rec differences lst =
  match lst with
  | x1 :: x2 :: rest -> (x2 - x1) :: differences (x2 :: rest)
  | _ -> []

let is_safe line =
  let diff = differences line in
  List.for_all (fun x -> x <= 3 && x >= 1) diff
  || List.for_all (fun x -> x >= -3 && x <= -1) diff

let rec remove_one lst =
  match lst with
  | [] -> []
  | x :: xs ->
      lst :: xs :: List.map (fun sublist -> x :: sublist) (remove_one xs)

let () =
  let filename = "./bin/day02/test.txt" in
  let lines = read_file filename in
  let parsed_lines = List.map parse_line lines in
  let removed_one = List.map remove_one parsed_lines in
  let res =
    List.map (fun x -> List.exists (fun line -> is_safe line) x) removed_one
    |> List.filter (fun x -> x)
    |> List.length
  in
  print_int res
