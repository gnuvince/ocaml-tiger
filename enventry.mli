type fun_entry = { fun_formals: Types.t list;
                   fun_result: Types.t }

type t =
  | VarEntry of Types.t
  | FunEntry of fun_entry

val to_string : t -> string
