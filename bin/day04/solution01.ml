let word = "XMAS"
let directions = [ (0, 1); (1, 0); (0, -1); (-1, 0); (-1, -1); (1, 1); (1, -1); (-1, 1) ]

let matches grid rows cols word x y (dx, dy) =
  let len = String.length word in
  let rec aux i =
    if i = len then true
    else
      let nx, ny = (x + (i * dx), y + (i * dy)) in
      if nx < 0 || ny < 0 || nx >= rows || ny >= cols then false
      else if grid.(nx).[ny] <> word.[i] then false
      else aux (i + 1)
  in
  aux 0

let count_occurrences grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let count = ref 0 in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      List.iter
        (fun dir -> if matches grid rows cols word x y dir then incr count)
        directions
    done
  done;
  !count

let () =
  let filename = "./bin/day04/test.txt" in
  let lines = Advent_of_ocaml_2024.Utils.read_file filename in
  let grid = Array.of_list lines in
  let total = count_occurrences grid in
  Printf.printf "Total occurrences of %s: %d\n" word total
