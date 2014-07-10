type unique = unit ref

type t =
  | Int
  | String
  | Nil
  | Unit
  | Record of (Sym.t * t) list * unique
  | Array of t * unique
  | Name of Sym.t * t option ref

let rec to_string = function
  | Int -> "int"
  | String -> "string"
  | Nil -> "nil"
  | Unit -> "()"
  | Name (sym, _) -> Sym.to_string sym
  | Record (fields, _) ->
     Printf.sprintf "{ %s }" (String.concat ", " (List.map field_to_string fields))
  | Array (typ, _) -> "array of " ^ to_string typ

and field_to_string (sym, typ) =
  Printf.sprintf "%s: %s"
    (Sym.to_string sym)
    (to_string typ)

let rec check expected actual =
  match (expected, actual) with
  | (Int, Int)                           -> true
  | (String, String)                     -> true
  | (Nil, Nil)                           -> true
  | (Unit, Unit)                         -> true
  | (Record (_, un1), Record (_, un2))   -> un1 == un2
  | (Array (_, un1), Array (_, un2))     -> un1 == un2
  | (Name (_, targ), other)
  | (other, Name (_, targ)) ->
     begin
       match !targ with
       | None -> false
       | Some ty -> check ty other
     end
  | _ -> false
