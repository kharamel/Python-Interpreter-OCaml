(* File main.ml 
  Author: Sushil Kharal
*)

#use "lexer.mml.ml";;

let symtable = Hashtbl.create 10;;

let rec main = fun tok_list -> fun n ->
  let s = if (size tok_list > 0) then tok_list else let _ = print_string ">>> " in tokenize (read_line ()) in
  try
  match s with
  _ when (check_exit s) -> print_string ""
  | x::xs when (is_if_statement [x]) ->
    let n = n + 1 in
    let _ = indent n in
    let to_eval = read_line() in 
    if (evaluate_if (x::xs) symtable) then main (tokenize to_eval) n else main [] 0
  | _ when (is_comparision s) -> let _ = Printf.printf "%s\n" (string_of_token [evaluate_comparision s symtable]) in main [] 0
  | _ when (is_assginment s) -> let _ = assign s symtable in main [] n
  | _ when (is_expression s) -> let _ = Printf.printf "%s\n" (string_of_token([eval_exp s symtable])) in main [] 0
  | _ -> main [] 0
  with
  | InvalidExpression msg -> let _ = Printf.printf "%s\n" msg in main [] 0
  | Not_found -> let _ = Printf.printf "Unassigned variable(s)\n" in main [] 0
  and
  indent = fun n ->
    match n with
    0 -> ""
    | _ -> let _ = print_string "\t" in indent (n - 1);;

main [] 0;;