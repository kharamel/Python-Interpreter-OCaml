(* File: token.ml *)

type token = Int_tok of int
            | Float_tok of float
            | Bool_tok of bool
            | Id_tok of string
            | String of string
            | Plus_tok
            | Minus_tok
            | Times_tok
            | Div_tok
            | Is_Eq_tok
            | Is_Neq_tok
            | Equal_tok
            | If_tok
            | Else_tok
            | Lparen_tok
            | Rparen_tok
            | Power_tok
            | Exit_tok
            | Col_tok
            ;;

type assoc = Left | Right;;

exception IgnoreCase;;
exception InvalidExpression of string;;

(* This function prints input token *)
let print_token = fun tok ->
match tok with
Int_tok x -> Printf.printf "Int_tok %d" x
| Float_tok x -> Printf.printf "Float_tok %F" x
| Bool_tok x -> Printf.printf "Bool_tok %B" x
| Id_tok x -> Printf.printf "Id_tok %s" x
| Plus_tok -> print_string "Plus_tok"
| Minus_tok -> print_string "Minus_tok"
| Div_tok -> print_string "Div_tok"
| Times_tok -> print_string "Times_tok"
| Is_Eq_tok -> print_string "Is_Eq_tok"
| Is_Neq_tok -> print_string "Is_Neq_tok"
| Equal_tok -> print_string "Equal_tok"
| If_tok -> print_string "If_tok"
| Else_tok -> print_string "Else_tok"
| Lparen_tok -> print_string "Lparen_tok"
| Rparen_tok -> print_string "Rparen_tok"
| Power_tok -> print_string "Power_tok"
| Exit_tok -> print_string "Exit_tok"
| Col_tok -> print_string "Col_tok"
| _ -> raise IgnoreCase;;

(* Give priority to operators *)
let priority = fun op ->
  match op with
  Power_tok -> 4
  | Times_tok -> 3
  | Div_tok -> 3
  | Plus_tok -> 2
  | Minus_tok -> 2
  | _ -> -1;;

let associativity = fun op ->
  match op with
  Power_tok -> Right
  | _ -> Left;;

(* Return integer value for boolean *)
let int_bool_val = fun b ->
  if b = true then 1
  else 0;;

(* Return integer value for boolean *)
let float_bool_val = fun b ->
  if b = true then 1.
  else 0.;;


(* This function prints list of token *)
let rec print_tokens = fun lst ->
match lst with
[] -> Printf.printf "\n"
| x::[] -> let _ = print_token x in print_tokens []
| x::xs -> let _ = print_token x in 
  let _ = print_string ", " in print_tokens xs;;


(* Check wheter the token is a number *)
let is_num = fun x ->
  match x with
  Int_tok x -> true
  | Float_tok x -> true
  | Bool_tok x -> true
  | _ -> false;;

  (* Check whether the token is an operator *)
let is_op = fun x ->
  match x with
  Power_tok -> true
  | Times_tok -> true
  | Div_tok -> true
  | Plus_tok -> true
  | Minus_tok -> true
  | _ -> false;;


(*  Return x^y *)
let power = fun x -> fun y ->
  match x, y with
  Int_tok x, Int_tok y -> Float_tok ((float_of_int x) ** (float_of_int y))
  | Float_tok x, Float_tok y -> Float_tok (x ** y)
  | Int_tok x, Float_tok y -> Float_tok ((float_of_int x) ** y)
  | Float_tok x, Int_tok y -> Float_tok (x ** (float_of_int y))
  | Bool_tok x, Bool_tok y -> Float_tok ((float_bool_val x) ** (float_bool_val y))
  | Bool_tok x, Int_tok y -> Float_tok ((float_bool_val x) ** (float_of_int y))
  | Int_tok x, Bool_tok y -> Float_tok ((float_of_int x) ** (float_bool_val y))
  | Bool_tok x, Float_tok y -> Float_tok ((float_bool_val x) ** y)
  | Float_tok x, Bool_tok y -> Float_tok (x ** (float_bool_val y));;

(* Multiply two values of int, float, or bool types *)
let times = fun x -> fun y ->
  match x, y with
  Int_tok x, Int_tok y -> Int_tok (x * y)
  | Float_tok x, Float_tok y -> Float_tok (x *. y)
  | Int_tok x, Float_tok y -> Float_tok ((float_of_int x) *. y)
  | Float_tok x, Int_tok y -> Float_tok (x *. (float_of_int y))
  | Bool_tok x, Bool_tok y -> Int_tok ((int_bool_val x) * (int_bool_val y))
  | Bool_tok x, Int_tok y -> Int_tok ((int_bool_val x) * y)
  | Int_tok x, Bool_tok y -> Int_tok (x * (int_bool_val y))
  | Bool_tok x, Float_tok y -> Float_tok ((float_bool_val x) *. y)
  | Float_tok x, Bool_tok y -> Float_tok (x *. (float_bool_val y));;

(* Divide two values of int, float, or bool type types *)
let div = fun x -> fun y ->
  match x, y with
  Int_tok x, Int_tok y -> Int_tok (x / y)
  | Float_tok x, Float_tok y -> Float_tok (x /. y)
  | Int_tok x, Float_tok y -> Float_tok ((float_of_int x) /. y)
  | Float_tok x, Int_tok y -> Float_tok (x /. (float_of_int y))
  | Bool_tok x, Bool_tok y -> Int_tok ((int_bool_val x) / (int_bool_val y))
  | Bool_tok x, Int_tok y -> Int_tok ((int_bool_val x) / y)
  | Int_tok x, Bool_tok y -> Int_tok (x / (int_bool_val y))
  | Bool_tok x, Float_tok y -> Float_tok ((float_bool_val x) /. y)
  | Float_tok x, Bool_tok y -> Float_tok (x /. (float_bool_val y));;

(* Add two values of int, float, or boot types *)
let add = fun x -> fun y ->
  match x, y with
  Int_tok x, Int_tok y -> Int_tok (x + y)
  | Float_tok x, Float_tok y -> Float_tok (x +. y)
  | Int_tok x, Float_tok y -> Float_tok ((float_of_int x) +. y)
  | Float_tok x, Int_tok y -> Float_tok (x +. (float_of_int y))
  | Bool_tok x, Int_tok y -> Int_tok ((int_bool_val x) + y)
  | Bool_tok x, Bool_tok y -> Int_tok ((int_bool_val x) + (int_bool_val y))
  | Int_tok x, Bool_tok y -> Int_tok (x + (int_bool_val y))
  | Bool_tok x, Float_tok y -> Float_tok ((float_bool_val x) +. y)
  | Float_tok x, Bool_tok y -> Float_tok (x +. (float_bool_val y));;

(* Subtract two values of int, float or bool type*)
let subtract = fun x -> fun y ->
  match x, y with
  Int_tok x, Int_tok y -> Int_tok (x - y)
  | Float_tok x, Float_tok y -> Float_tok (x -. y)
  | Int_tok x, Float_tok y -> Float_tok ((float_of_int x) -. y)
  | Float_tok x, Int_tok y -> Float_tok (x -. (float_of_int y))
  | Bool_tok x, Int_tok y -> Int_tok ((int_bool_val x) - y)
  | Bool_tok x, Bool_tok y -> Int_tok ((int_bool_val x) - (int_bool_val y))
  | Int_tok x, Bool_tok y -> Int_tok (x - (int_bool_val y))
  | Bool_tok x, Float_tok y -> Float_tok ((float_bool_val x) -. y)
  | Float_tok x, Bool_tok y -> Float_tok (x -. (float_bool_val y));;

(* get priority of element at the head of the list *)
let priority_head = fun lst ->
  match lst with
  [] -> -1
  | x::xs -> priority x;;

let is_empty = fun lst ->
  match lst with
  [] -> true
  | _ -> false;;

let size = fun lst ->
  let rec size = fun lst -> fun i ->
    match lst with
    [] -> i
    | x::xs -> size xs (i + 1)
  in
  size lst 0;;

(* Get element at the head of list !!!ASSUMING EMPTY LIST IS NOT PASSED !!!*)
let head = fun lst ->
  match lst with
  | x::xs -> x
  | _ -> raise (InvalidExpression "The operation is invalid");;

let pop = fun lst ->
  match lst with
  [] -> []
  | x::xs -> xs;;

(* Pop list until Lparen token is seen *)
let rec pop_till_lparen = fun lst ->
  match lst with
  [] -> raise (InvalidExpression "The operation is invalid")
  | (Lparen_tok)::xs -> xs
  | x::xs -> pop_till_lparen xs;;

(* add operators into another list until Lparen token is seen *)
let rec add_till_lparen = fun op_list -> fun rpn_list ->
  match op_list with
  | (Lparen_tok)::xs -> rpn_list
  | x::xs -> add_till_lparen xs (rpn_list @ [x]);;

let left_assos_leq = fun op1 -> fun op2 ->
  if ((priority op1) <= (priority op2)) && (associativity op1 = Left) then 
    (* let _, _ = print_string " lleq\n", (print_tokens [op1;op2]) in  *)
    true
  else false

let right_assos_less = fun op1 -> fun op2 ->
  if ((priority op1) < (priority op2)) && (associativity op1 = Right) then 
    (* let _, _ = print_string " rless\n", (print_tokens [op1;op2]) in  *)
    true
  else false

(* Get operator and two values and apply operator on those two values *)
let operate = fun op -> fun x -> fun y ->
  match op with
  Power_tok -> power x y
  | Times_tok -> times x y
  | Div_tok -> div x y
  | Plus_tok -> add x y
  | Minus_tok -> subtract x y;;

(* Look up for token in symtable and return it's value *)
let lookup = fun symtable -> fun id -> Hashtbl.find symtable id;;

(* update symbol table for the id with given value *)
let update = fun symtable -> fun id -> fun val_ ->
  if (Hashtbl.mem symtable id) then (Hashtbl.replace symtable id val_)
  else (Hashtbl.add symtable id val_);;

(* Check if the list of token generated by user input is an assignment of value *)
let rec is_assginment = fun token_list ->
  match token_list with
  [] -> false
  | (Equal_tok)::xs -> true
  | x::xs -> is_assginment xs;;

(* Get RPN by using shunting yard algorithm *)
let get_rpn = fun expn -> fun symtable ->
  let rec get_rpn = fun token_list -> fun op_list -> fun rpn_list -> fun symtable ->
    match token_list with
    [] -> rpn_list @ op_list
    | x::xs when is_num x -> 
      (* let _, _ = print_string " found number\n", print_token x in  *)
      get_rpn xs op_list (rpn_list @ [x]) symtable
    | x::xs when (is_op x) && (not (is_empty op_list)) && ((left_assos_leq x (head op_list)) || (right_assos_less x (head op_list))) -> 
      (* let _, _, _ =  print_tokens rpn_list, print_tokens op_list, print_string " tokens so far\n" in  *)
      get_rpn (x::xs) ((pop op_list)) (rpn_list @ [head op_list]) symtable
    | (Lparen_tok)::xs -> get_rpn xs (Lparen_tok::op_list) rpn_list symtable
    | (Rparen_tok)::xs -> get_rpn xs (pop_till_lparen op_list) (add_till_lparen op_list rpn_list) symtable
    | x::xs when (is_op x) -> 
      (* let _, _ = print_token x, print_string "\n" in  *)
      get_rpn xs (x::op_list) rpn_list symtable
    | (Id_tok x)::xs -> get_rpn xs op_list (rpn_list @ [lookup symtable (Id_tok x)]) symtable
    | x::xs -> let _ = print_token x in raise IgnoreCase
  in
  get_rpn expn [] [] symtable;;

(* Return string of token *)
let string_of_token = fun x ->
  match x with
  Int_tok x::[] -> string_of_int x
  | Float_tok x::[] -> string_of_float x
  | Bool_tok x::[] when x = true -> "True"
  | Bool_tok x::[] when x = false -> "False"
  | _ -> "Invalid Expression";;

(* This function evaluates the expression *)
let eval_rpn = fun token_list ->
  let rec eval_rpn = fun token_list -> fun result_list ->
    match token_list with
    [] when (size result_list <> 1) -> raise (InvalidExpression "Invalid expression")
    | [] -> (head result_list)
    | x::xs when (is_op x) -> 
      (* let _, _ = print_string " found operator\n", print_token x in  *)
      let b = head result_list in
      let result_list = pop result_list in
      let a = head result_list in
      let result_list = pop result_list in
      let ans = operate x a b in 
      (* let _ = print_token ans in *)
      eval_rpn xs (ans::result_list)
    | x::xs when (is_num x) -> 
(*       let _, _ = print_string " found num\n", print_token x in  *)
      eval_rpn xs (x::result_list)
    | _ -> raise (InvalidExpression "The expression is invalid")
    in
    eval_rpn token_list [];;

(* This function receives an expression and Evaluates it *)
let eval_exp = fun exp -> fun symtable ->
  let lst = (get_rpn exp symtable) in
  (eval_rpn lst);;

(* Skip until a token is found *)
let rec skip_until = fun lst -> fun tok -> 
  match lst with
  [] -> raise (InvalidExpression "The expression is invalid")
  | x::xs when (x = tok) ->  xs
  | x::xs -> skip_until xs tok;;

(* Skip everything after the token *)
let skip_after = fun lst -> fun tok -> 
  let rec skip_after = fun lst -> fun tok -> fun lst2 ->
  match lst with
  [] -> raise (InvalidExpression "The expression is invalid")
  | x::xs when (x = tok) ->  lst2
  | x::xs -> skip_after xs tok (lst2 @ [x])
  in
  skip_after lst tok [];;

(* Return the first comparision token found in the list *)
let rec get_comp_tok = fun lst ->
  match lst with
  [] -> raise (InvalidExpression "The expression is invalid")
  | Is_Eq_tok::xs -> Is_Eq_tok
  | Is_Neq_tok::xs -> Is_Neq_tok
  | x::xs -> get_comp_tok xs;;

(* This function receives list of token with if statement and evaluates them *)
let rec evaluate_if = fun tok_list -> fun symtable ->
  match tok_list with
  If_tok::xs ->
    let lhs = skip_after xs (get_comp_tok xs) in
    let rhs = skip_after (skip_until xs (get_comp_tok xs)) (Col_tok) in 
    (eval_exp lhs symtable) = (eval_exp rhs symtable)
  | _ -> false;;


let evaluate_comparision = fun tok_list -> fun symtable ->
  let to_skip = get_comp_tok tok_list in
  let lhs = skip_after tok_list to_skip in
  let rhs = skip_until tok_list to_skip in
  if (to_skip = Is_Eq_tok) then
    if (eval_exp lhs symtable) = (eval_exp rhs symtable) then (Bool_tok true)
    else Bool_tok false
  else
    if (eval_exp lhs symtable) = (eval_exp rhs symtable) then (Bool_tok false)
    else Bool_tok true;;


(* Assign value to the identifier *)
let assign = fun token_lst -> fun symtable ->
  match token_lst with
  (Id_tok x)::(Equal_tok)::xs ->
    let lhs = Id_tok x in
    let rhs = eval_exp xs symtable in
    update symtable lhs rhs
  | _ -> raise (InvalidExpression "Invalid assignment");;


(* Check if to exit the program *)
let check_exit = fun tok_list -> 
  match tok_list with
  Exit_tok::[] -> true
  | _ -> false;;

(* Check if the list of token is an expression *)
let is_expression = fun tok_list ->
  match tok_list with
  [] -> false
  | _ -> true;;

(* Check if the statement is comparision *)
let rec is_comparision = fun tok_lst ->
  match tok_lst with
  [] -> false
  | Is_Eq_tok::xs -> true
  | Is_Neq_tok::xs -> true
  | x::xs -> is_comparision xs;;

(* Check if thoken list is an if statement *)
let is_if_statement = fun tok_list ->
  match tok_list with
  If_tok::xs -> true
  | _ -> false