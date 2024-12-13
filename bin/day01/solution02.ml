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

let parse_line line =
  match String.split_on_char ' ' line |> List.filter ((<>) "") |> List.map int_of_string with
  | [x; y] -> (x, y)
  | _ -> failwith "Invalid input format"

let split_pairs pairs =
  List.fold_right (fun (x, y) (left, right) -> (x :: left, y :: right)) pairs ([], [])

let count_occurrences lst =
  List.fold_left (fun acc x ->
    let count = try List.assoc x acc with Not_found -> 0 in
    (x, count + 1) :: List.remove_assoc x acc
  ) [] lst

let similarity_score left right =
  let right_counts = count_occurrences right in
  List.fold_left (fun acc x ->
    let count = try List.assoc x right_counts with Not_found -> 0 in
    acc + (x * count)
  ) 0 left

let () =
  let filename = "./bin/day01/test.txt" in
  let lines = read_file filename in
  let pairs = List.map parse_line lines in
  let (left, right) = split_pairs pairs in
  let score = similarity_score left right in
  Printf.printf "Similarity score: %d\n" score