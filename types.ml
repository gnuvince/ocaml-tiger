type unique = unit ref

type t =
  | Int
  | String
  | Record of (Sym.t * t) list * unique
  | Array of t * unique
  | Nil
  | Unit
  | Name of Sym.t * t option ref
