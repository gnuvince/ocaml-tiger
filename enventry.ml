type t =
  | VarEntry of Types.t
  | FunEntry of fun_entry

and fun_entry = { fun_formals: Types.t list;
                  fun_result: Types.t }

let to_string = function
  | VarEntry t -> Printf.sprintf "<var: %s>" (Types.to_string t)
  | FunEntry { fun_formals; fun_result } ->
     Printf.sprintf "<fun: (%s) -> %s>"
       (String.concat ", " (List.map Types.to_string fun_formals))
       (Types.to_string fun_result)
