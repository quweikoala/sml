(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun only_capitals (sl) =
  List.filter (fn str => Char.isUpper(String.sub(str, 0))) sl

val q1 = only_capitals(["Qu", "qu", "HUI", "aE"])

fun longest_string1 (sl) =
  List.foldl (fn (cur_long, str) => if String.size cur_long > String.size str
                                    then cur_long
                                    else str)
             "" sl

val q2_a = longest_string1(["a", "Qu", "qu", "aE"])
val q2_b = longest_string1(["a", "Qu", "qu", "aEE"])

fun longest_string2 (sl) = 
  List.foldl (fn (cur_long, str) => if String.size cur_long >= String.size str
                                    then cur_long
                                    else str)
             "" sl

val q3_a = longest_string2(["a", "Qu", "qu", "aE"])
val q3_b = longest_string2(["a", "Qu", "qu", "aEE"])

fun longest_string_helper cmp sl =
  List.foldl (fn (cur_long, str) => if cmp(cur_long, str)
                                    then cur_long
                                    else str) "" sl

val longest_string3 = longest_string_helper (fn (x, y) => String.size x > String.size y)
val q4_a = longest_string3(["a", "Qu", "qu", "aE"])
val q4_b = longest_string3(["a", "Qu", "qu", "aEE"])

val longest_string4 = longest_string_helper (fn (x, y) => String.size x >= String.size y)
val q4_c = longest_string4(["a", "Qu", "qu", "aE"])
val q4_d = longest_string4(["a", "Qu", "qu", "aEE"])

val longest_capitalized  = longest_string2 o only_capitals
val q5_c = longest_capitalized(["a", "Qu", "qu", "aEF"])
val q5_d = longest_capitalized(["a", "Qu", "qu", "AEEx"])

fun rev_string (str) =
  String.implode(List.rev(String.explode(str)))

val q6 = rev_string("jfiqjfoqoeifqie")

fun first_answer to_opt array =
  case array of
    [] => raise NoAnswer
  | ele::array' => case to_opt(ele) of
                     NONE => first_answer to_opt array'
                   | SOME v => v

fun test_opt v =
  case v of
    1 => NONE
 |  2 => SOME 2
 |  3 => SOME 3
 |  _ => NONE
val q7_a = first_answer test_opt [1,2,1,2,3,4]
(* val q7_b = first_answer test_opt [1,1,4] *)

fun all_answers to_opt array =
  let fun aux (array, acc) =
        case array of 
          [] => acc
        | ele::array' => case to_opt(ele) of
                            NONE => aux(array', acc)
                          | SOME v => aux(array', v::acc)
      val result = aux(array, [])
  in
    if List.length array = 0 orelse List.length result <> 0 then SOME result else NONE 
  end

val q8_a = all_answers test_opt [1,2,1,2,3,4]
val q8_b = all_answers test_opt [1,1,4]
val q8_c = all_answers test_opt [1,1,4]
