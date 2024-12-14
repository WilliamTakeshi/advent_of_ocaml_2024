let () =
  let filename = "./bin/day03/test.txt" in
  let lines = Advent_of_ocaml_2024.Utils.read_file filename in
  List.iter print_endline lines
