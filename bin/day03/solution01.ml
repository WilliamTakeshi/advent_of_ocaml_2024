let find_all_matches input =
  let open Re in
  let open Option in
  let pattern =
    compile
      (seq
         [
           str "mul(";
           repn digit 1 (some 3);
           str ",";
           repn digit 1 (some 3);
           str ")";
         ])
  in
  let digits_pattern = compile (repn digit 1 (some 3)) in
  let muls = List.map (fun group -> Group.get group 0) (all pattern input) in
  List.map
    (fun x ->
      List.map
        (fun group -> int_of_string (Group.get group 0))
        (all digits_pattern x))
    muls

let my_mult input =
  match input with [ x; y ] -> x * y | _ -> failwith "Invalid input"

let () =
  let filename = "./bin/day03/input.txt" in
  let lines = Advent_of_ocaml_2024.Utils.read_file filename in
  let matches = List.map find_all_matches lines in
  let res = List.flatten (List.map (List.map my_mult) matches) in
  let res2 = List.fold_right (fun acc a -> a + acc) res 0 in
  print_int res2
