(* get history from utop *)
#require "lambda-term";;
List.iter
  (fun e -> Printf.printf "%s\n" (Zed_string.to_utf8 e))
  (LTerm_history.contents UTop.history)
;;
