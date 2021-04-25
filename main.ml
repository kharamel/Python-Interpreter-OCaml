(* File main.ml *)

#use "lexer.mml.ml";;

(* let rec main = fun x -> 
  match x with
    0 -> read_line ()
    | _ ->  let _ = print_tokens (tokenize (read_line())) in main (x -1);;

main 100;; *)
let s = read_line ();;
let s = get_rpn (tokenize s);;
let r = eval_rpn s;;
print_string r;;