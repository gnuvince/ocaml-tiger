open Printf

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
  | Name (sym, targ) ->
     Printf.sprintf "%s -> %s"
       (Sym.to_string sym)
       (match !targ with
       | None -> "?"
       | Some t -> to_string t
       )
  | Record (fields, _) ->
     Printf.sprintf "{ %s }" (Utils.join ", " field_to_string fields)
  | Array (typ, _) -> "array of " ^ to_string typ

and field_to_string (sym, typ) =
  Printf.sprintf "%s: %s"
    (Sym.to_string sym)
    (to_string typ)

let rec actual_type = function
  | Int -> Int
  | String -> String
  | Nil -> Nil
  | Unit -> Unit
  | Record (fields, un) -> Record (List.map (fun (name, typ) -> (name, actual_type typ)) fields, un)
  | Array (typ, un) -> Array (actual_type typ, un)
  | Name (name, targ) ->
     (match !targ with
     | None -> failwith (Printf.sprintf "type '%s' is not initialized" (Sym.to_string name))
     | Some typ -> actual_type typ)

let rec check expected actual =
  match (expected, actual) with
  | (Int, Int)                           -> true
  | (String, String)                     -> true
  | (Nil, Nil)                           -> true
  | (Unit, Unit)                         -> true
  | (Record (_, un1), Record (_, un2))   -> un1 == un2 (* Check fields? *)
  | (Array (_, un1), Array (_, un2))     -> un1 == un2 (* Check element type? *)
  | (Name (_, targ), other)
  | (other, Name (_, targ)) ->
     begin
       match !targ with
       | None -> false
       | Some ty -> check ty other
     end
  | _ -> false
