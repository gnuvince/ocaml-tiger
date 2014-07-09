let base_tenv =
  let open Symtable in
  empty
  |> add (Sym.from_string "int") Types.Int
  |> add (Sym.from_string "string") Types.String


let base_venv =
  let func params result =
    Enventry.FunEntry { fun_formals=params; fun_result=result }
  in
  let open Symtable in
  let open Types in
  empty
  |> add (Sym.from_string "print")     (func [String] Unit)
  |> add (Sym.from_string "flush")     (func [] Unit)
  |> add (Sym.from_string "getchar")   (func [] String)
  |> add (Sym.from_string "ord")       (func [String] Int)
  |> add (Sym.from_string "chr")       (func [Int] String)
  |> add (Sym.from_string "size")      (func [String] Int)
  |> add (Sym.from_string "substring") (func [String; Int; Int] String)
  |> add (Sym.from_string "concat")    (func [String; String] String)
  |> add (Sym.from_string "not")       (func [Int] Int)
  |> add (Sym.from_string "exit")      (func [Int] Unit)
