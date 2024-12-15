let is_mas_x grid rows cols x y =
  let top = x - 1 in
  let bottom = x + 1 in
  let left = y - 1 in
  let right = y + 1 in
  if top < 0 || bottom >= rows || left < 0 || right >= cols then false
  else
    grid.(x).[y] = 'A'
    && ((grid.(top).[left] = 'M' && grid.(bottom).[right] = 'S') || (grid.(top).[left] = 'S' && grid.(bottom).[right] = 'M'))
    && ((grid.(top).[right] = 'M' && grid.(bottom).[left] = 'S') || (grid.(top).[right] = 'S' && grid.(bottom).[left] = 'M'))


let count_mas_x grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let count = ref 0 in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      if is_mas_x grid rows cols x y then incr count
    done
  done;
  !count

let () =
  let filename = "./bin/day04/input.txt" in
  let lines = Advent_of_ocaml_2024.Utils.read_file filename in
  let grid = Array.of_list lines in
  let total = count_mas_x grid in
  Printf.printf "Total MAS 'X' patterns: %d\n" total
