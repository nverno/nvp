(* get history from utop *)
#require "lambda-term"
#require "str"

module LTerm_history = struct
  include LTerm_history
      
  let rec remove_dups lst = 
    match lst with
    | [] -> []
    | h :: t -> (remove_dups (List.filter (x -> x != h) t)) ;;
                
  (* drop anything w/ len < 5, remove trailing newlines and ';;' *)
  let clean_hist () =
    let re = Str.regexp "^[ \n]+\|[ \r\n;]+$" in
    let qre = Str.regexp "\"" in
    List.map
      (fun s ->
         Str.replace_first re "" 
           (Str.global_replace qre "\\\\\"" (Zed_string.to_utf8 s)))
      (List.filter (fun s -> Zed_string.length s > 6)
         (* FIXME: doesn't remove dups as it should *)
         (remove_dups (LTerm_history.contents UTop.history)))

  (* dump history in REPL, most recent entry last *)
  let dump () =
    List.iter
      (fun e -> Printf.printf "%s\n" e)
      (List.rev (clean_hist ()))

  (* dump as readable lisp list *)
  let dump_lisp () =
    Printf.printf "(";
    List.iter
      (fun e -> Printf.printf "\"%s\"\n" e)
      (clean_hist ());
    Printf.printf ")"

end
