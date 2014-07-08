let base_tenv =
  let open Symtable in
  empty
  |> add "int" Types.Int
  |> add "string" Types.String


let base_venv =
  let func params result =
    Enventry.FunEntry { fun_formals=params; fun_result=result }
  in
  let open Symtable in
  let open Types in
  empty
  |> add "print"     (func [String] Unit)
  |> add "flush"     (func [] Unit)
  |> add "getchar"   (func [] String)
  |> add "ord"       (func [String] Int)
  |> add "chr"       (func [Int] String)
  |> add "size"      (func [String] Int)
  |> add "substring" (func [String; Int; Int] String)
  |> add "concat"    (func [String; String] String)
  |> add "not"       (func [Int] Int)
  |> add "exit"      (func [Int] Unit)
