open OUnit
open Types

let test_check_basic _ =
  assert_equal true (check Int Int);
  assert_equal true (check String String);
  assert_equal true (check Unit Unit);
  assert_equal true (check Nil Nil);

  assert_equal false (check Int String);
  assert_equal false (check Int Unit);
  assert_equal false (check Int Nil);

  assert_equal false (check String Int);
  assert_equal false (check String Unit);
  assert_equal false (check String Nil);

  assert_equal false (check Unit Int);
  assert_equal false (check Unit String);
  assert_equal false (check Unit Nil);

  assert_equal false (check Nil Int);
  assert_equal false (check Nil String);
  assert_equal false (check Nil Unit)


let test_check_array _ =
  let uniq1 = ref () in
  let uniq2 = ref () in

  let arr1 = Array (Int, uniq1) in
  let arr2 = Array (Int, uniq1) in
  let arr3 = Array (Int, uniq2) in
  let arr4 = Array (String, uniq1) in

  assert_equal true (check arr1 arr1);
  assert_equal true (check arr1 arr2);
  assert_equal false (check arr1 arr3);
  assert_equal false (check arr2 arr3);
  assert_equal true (check arr1 arr4); (* Only the unique ref is checked. *)
  assert_equal false (check arr3 arr4)


let test_check_record _ =
  let uniq1 = ref () in
  let uniq2 = ref () in

  let sym = Sym.from_string in
  let rec1 = Record ([(sym "x", Int); (sym "y", Int)], uniq1) in
  let rec2 = Record ([(sym "x", Int); (sym "y", Int)], uniq1) in
  let rec3 = Record ([(sym "x", Int); (sym "y", Int)], uniq2) in
  let rec4 = Record ([(sym "x", Int); (sym "y", String)], uniq1) in

  assert_equal true (check rec1 rec1);
  assert_equal true (check rec1 rec2);
  assert_equal false (check rec1 rec3);
  assert_equal true (check rec1 rec4) (* Only the unique ref is checked. *)


let test_check_name _ =
  let sym = Sym.from_string in
  let name1 = Name (sym "n1", ref (Some Int)) in
  let name2 = Name (sym "n2", ref (Some String)) in
  let name3 = Name (sym "n3", ref (Some name1)) in
  let name4 = Name (sym "n4", ref None) in

  assert_equal true (check name1 Int);
  assert_equal true (check Int name1);
  assert_equal true (check name1 name1);
  assert_equal true (check name1 name3);
  assert_equal true (check name3 name1);
  assert_equal true (check Int name3);
  assert_equal true (check name3 Int);
  assert_equal false (check name1 name2);
  assert_equal false (check name2 name1);
  assert_equal false (check name2 name3);
  assert_equal false (check name3 name2);
  assert_equal false (check name1 name4);
  assert_equal false (check name2 name4);
  assert_equal false (check name3 name4);
  assert_equal false (check name4 name4);
  assert_equal false (check Int name4)


let suite =
  "types" >::: [
    "test_check_basic" >:: test_check_basic;
    "test_check_array" >:: test_check_array;
    "test_check_record" >:: test_check_record;
    "test_check_name" >:: test_check_name;
  ]

let _ =
  run_test_tt_main suite
