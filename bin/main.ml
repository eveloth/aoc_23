exception Invalid_input

let get_filename day =
  let input_dir = "inputs" in
    input_dir ^ "/" ^ string_of_int day ^ ".txt"


let rec list_from_file lst ic = 
    match input_line ic with
    | line -> list_from_file(line :: lst) ic
    | exception End_of_file -> close_in ic; List.rev lst


let usage = "aoc_23 <day>"
let day = ref ""
let anon_fun dayArg = day := dayArg

let alphabet = ['1'; '2'; '3'; '4'; '5'; '6';'7'; '8'; '9'; '0'] 
let last list = list |> List.rev |> List.hd
let edges list = List.hd list :: last list :: []

let sum file = file 
   |> List.map (fun s -> s |> String.to_seq |> Seq.filter (fun c -> alphabet |> List.mem c) |> String.of_seq)
   |> List.filter (fun s -> String.length s > 0)
   |> List.map (fun s -> s |> String.to_seq |> List.of_seq |> edges |> List.to_seq |> String.of_seq)
   |> List.map (fun s -> int_of_string s)
   |> List.fold_left ( + ) 0
   |> string_of_int
   |> print_endline

let day_1() = 
       let ic = open_in (get_filename 1) in 
         let file = list_from_file [] ic in sum file

let get_daily_processor day =
  match day with 1 -> day_1 | _ -> raise Invalid_input

let run day = get_daily_processor day

let spec = []

let parse_day str =
  match str with "" -> raise Invalid_input | s -> int_of_string s

let () =
  Arg.parse spec anon_fun usage;
  print_endline ("You've chosen day " ^ !day);
  (parse_day !day |> run)()
