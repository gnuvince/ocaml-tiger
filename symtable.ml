module SymMap = Map.Make(struct
  type t = Sym.t
  let compare a b = Sym.compare a b
end)


type 'a t = 'a SymMap.t

let empty = SymMap.empty

let add sym value t =
  SymMap.add sym value t

let find sym t =
  try
    Some (SymMap.find sym t)
  with Not_found ->
    None

let print conv t =
  SymMap.iter (fun k v ->
    Printf.printf "%s: %s\n" (Sym.to_string k) (conv v))
    t
