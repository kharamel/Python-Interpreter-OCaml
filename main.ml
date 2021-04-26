(* File main.ml *)

#use "lexer.mml.ml";;

let symtable = Hashtbl.create 10;;

let rec main = fun tok_list ->
  let s = if (size tok_list > 0) then tok_list else let _ = print_string ">>> " in tokenize (read_line ()) in
  try
  match s with
  _ when (check_exit s) -> print_string ""
  | x::xs when (is_if_statement [x]) -> if (evaluate_if (x::xs) symtable) then main (skip_until (x::xs) Col_tok) else main []
  | _ when (is_comparision s) -> let _ = Printf.printf "%s\n" (string_of_token [evaluate_comparision s symtable]) in main []
  | _ when (is_assginment s) -> let _ = assign s symtable in main []
  | _ when (is_expression s) -> let _ = Printf.printf "%s\n" (string_of_token([eval_exp s symtable])) in main []
  | _ -> main []
  with
  | InvalidExpression msg -> let _ = Printf.printf "%s\n" msg in main []
  | Not_found -> let _ = Printf.printf "Unassigned variable(s)\n" in main [];;

main [];;