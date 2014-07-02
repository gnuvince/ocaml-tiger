type t = {
  filename   : string;
  start_line : int;
  start_col  : int;
  end_line   : int;
  end_col    : int;
}

let create filename start_line start_col end_line end_col =
  { filename; start_line; start_col; end_line; end_col }

let to_string pos =
  Printf.sprintf "%s@%d:%d-%d:%d"
    pos.filename pos.start_line pos.start_col pos.end_line pos.end_col
