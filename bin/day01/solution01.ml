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

let () =
  let filename = "./bin/day01/test.txt" in
  match read_file filename with
  | [] -> Printf.printf "The file is empty.\n"
  | lines -> List.iter print_endline lines