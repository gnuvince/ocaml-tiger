type t =
  | VarEntry of Types.t
  | FunEntry of fun_entry

and fun_entry = { fun_formals: Types.t list;
                  fun_result: Types.t }
