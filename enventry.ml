type fun_entry = { fun_formals: Types.t list;
                   fun_result: Types.t }

type t =
  | VarEntry of Types.t
  | FunEntry of fun_entry

let to_string = function
  | VarEntry t -> Printf.sprintf "<var: %s>" (Types.to_string t)
  | FunEntry { fun_formals; fun_result } ->
     Printf.sprintf "<fun: (%s) -> %s>"
       (Utils.join ", " Types.to_string fun_formals)
       (Types.to_string fun_result)
