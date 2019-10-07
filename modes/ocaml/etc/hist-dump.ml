(* get history from utop *)
#require "lambda-term"
#require "str"

module LTerm_history = struct
  include LTerm_history
      
  let rec remove_dups lst = 
    match lst with
    | [] -> []
    | h :: t -> h :: (remove_dups (List.filter (fun x -> x != h) t))
              
  (* drop anything w/ len < 5, remove trailing newlines and ';;' *)
  let clean_hist () =
    let re = Str.regexp "^[ \n]+|[ \r\n;]+$" in
    (* let qre = Str.regexp "\"" in *)
    (* remove_dups *)
    (List.map
       (fun s ->
          Str.replace_first re "" (String.escaped (Zed_string.to_utf8 s)))
       (List.filter (fun s -> Zed_string.length s > 6)
          (LTerm_history.contents UTop.history)))

  (* dump history in REPL, most recent entry last *)
  let dump () =
    let module Z = Zed_string in
    List.iter
      (fun s -> Z.to_utf8 s |> String.escaped |> Printf.printf "\"%s\"\n")
      (List.rev (LTerm_history.contents UTop.history))

  (* dump as readable lisp list *)
  let dump_lisp () =
    Printf.printf "(";
    List.iter
      (fun e -> Printf.printf "\"%s\"\n" e)
      (clean_hist ());
    Printf.printf ")\n"

end
