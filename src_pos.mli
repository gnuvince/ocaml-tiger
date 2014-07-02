(* Start/end position of in a source file. *)
type t = {
  filename   : string;
  start_line : int;
  start_col  : int;
  end_line   : int;
  end_col    : int;
}

val create : string -> int -> int -> int -> int -> t
val to_string : t -> string
