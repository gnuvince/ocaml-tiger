type unique = unit ref

type t =
  | Int
  | String
  | Nil
  | Unit
  | Record of (Sym.t * t) list * unique
  | Array of t * unique
  | Name of Sym.t * t option ref


val to_string : t -> string
val check : t -> t -> bool
val actual_type : t -> t
