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
let is_digit c = List.mem c alphabet
let get_numeric_part str = String.fold_left (fun acc c -> acc ^ match is_digit c with true -> String.make 1 c | false -> "") "" str 
let last list = list |> List.rev |> List.hd
let edges list = List.hd list :: last list :: []

let split2 c str = let list = String.split_on_char c str in
  match list with
  | [] -> raise (Invalid_argument "no delimeter found")
  | x :: xs -> (x, List.fold_left (fun curr next -> curr ^ next) "" xs)

let sum file = file 
   |> List.map (fun s -> get_numeric_part s)
   |> List.filter (fun s -> String.length s > 0)
   |> List.map (fun s -> s |> String.to_seq |> List.of_seq |> edges |> List.to_seq |> String.of_seq)
   |> List.map (fun s -> int_of_string s)
   |> List.fold_left ( + ) 0
   |> string_of_int
   |> print_endline

type game = { id: int; red: int; green: int; blue: int }
let game_boundaries = { id = 0; red = 13; green = 14; blue = 15 }

let get_game_id str = int_of_string @@ get_numeric_part str

let game_particles str = 
        String.fold_left(fun acc c -> acc ^ match c with ';' -> "," | _ -> String.make 1 c) "" str
        |> String.split_on_char ',' |> List.map (fun s -> String.trim s)

let raw_game str =
  let parts = String.split_on_char ':' str in
  let particles = game_particles @@ List.hd @@ List.tl parts in
  let game_id = get_game_id @@ List.hd @@  parts in
  let game = {id = game_id; red = 0; green = 0; blue = 0 } in 
        (game, particles)

let update_red game newValue = match game.red < newValue with
  | true -> { game with red = newValue }
  | false -> game
let update_green game newValue = match game.green < newValue with
  | true -> { game with green = newValue }
  | false -> game
let update_blue  game newValue = match game.blue < newValue with
  | true -> { game with blue = newValue }
  | false -> game

let get_round_result game round =
  let tuple = split2 ' ' round in
  let first = fst tuple in
  let second = snd tuple in
  match second with 
  | "red" -> update_red game @@ int_of_string first 
  | "green" -> update_green game @@ int_of_string first 
  | "blue" -> update_blue game @@ int_of_string first   
  | _ -> raise (Invalid_argument second)

let rec count_cubes game roundList =
  match roundList with
  | [] -> game
  | x :: xs -> let roundResult = get_round_result game x in count_cubes roundResult xs

let parse_game str = 
  let rawGame = raw_game str in 
  let roundList = snd rawGame in
  let game = fst rawGame in
  count_cubes game roundList

let get_game_result input = 
  input 
  |> List.map (fun x -> parse_game x)
  |> List.filter (fun x -> 
    x.red < game_boundaries.red && 
    x.green  < game_boundaries.green &&  
    x.blue < game_boundaries.blue)
  |> List.map (fun x -> x.id)
  |> List.fold_left (fun curr next -> curr + next) 0
  |> string_of_int
  |> print_endline



let day_1() = 
       let ic = open_in (get_filename 1) in 
         let file = list_from_file [] ic in sum file

let day_2() = 
       let ic = open_in (get_filename 2) in 
         let file = list_from_file [] ic in get_game_result file

let get_daily_processor day =
  match day with 
  |1 -> day_1 
  |2 -> day_2 
  | _ -> raise Invalid_input

let run day = get_daily_processor day

let spec = []

let parse_day str =
  match str with "" -> raise Invalid_input | s -> int_of_string s

let () =
  Arg.parse spec anon_fun usage;
  print_endline ("You've chosen day " ^ !day);
  (parse_day !day |> run)()
