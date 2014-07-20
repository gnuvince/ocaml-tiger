type t = (string * int)

let next = ref 0

let syms : (string, int) Hashtbl.t = Hashtbl.create 97

let compare (_, a) (_, b) =
  Pervasives.compare a b

let to_string (sym, _) =
  sym

let from_string str =
  try
    let id = Hashtbl.find syms str in
    (str, id)
  with Not_found ->
    let sym = (str, !next) in
    Hashtbl.add syms str !next;
    incr next;
    sym
