(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["A","bc","C","cd"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 ["A","bc","C","ce"] = "ce"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

(* val test12 = first_match Unit [UnitP] = SOME [] *)

val test11 = match (Const(1), UnitP) = NONE
val test1101 = match (Const(1), ConstP 1) = SOME []
val test1102 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val test1103 = match (Const(1), TupleP [Wildcard]) = NONE
val test1104 = match (Const(1), TupleP [ConstP 1]) = NONE
val test1105 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val test1106 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []
val test1107 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val test1108 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []
val test1109 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val test1110 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]]) = NONE
val test1111 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]]) = NONE


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test701 = first_answer (fn x => if x > 3 then SOME x else NONE) [4,2,3,5] = 4
val test702 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] ; false) handle NoAnswer => true
val test7022 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] ; false) handle OtherException => true
val test703 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,2] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test801 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [3,2,4,5,6,7] = NONE
val test802 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,5,6,8] = NONE
val test803 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,6,8] = SOME [2,4,6,8]
val test804 = all_answers (fn x => if x mod 2 = 0 then SOME [x, x + 1] else NONE) [2,4,6,8] = SOME [2,3,4,5,6,7,8,9]
val test805 = all_answers (fn x => if x mod 2 = 0 then SOME [] else NONE) [2,4,6,8] = SOME []
val test806 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [] = SOME []



val test12 = first_match Unit [UnitP] = SOME []
val test1201 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]
val test1202 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]
val test1203 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE

