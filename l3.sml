(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(* SECOND *)
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

val count_wildcards =
  g (fn () => 1) (fn (_) => 0)

val q9a_1 = count_wildcards(TupleP ([Wildcard, UnitP, ConstructorP ("abc", Wildcard)]))
  
val count_wild_and_variable_lengths =
  g (fn () => 1) (fn (x) => String.size x)

val q9b_1 = count_wild_and_variable_lengths(TupleP ([Wildcard, UnitP, ConstructorP ("abc", Wildcard), Variable "ax"]))

fun count_some_var (str, ptn) = 
  g (fn () => 0) (fn (x) => if x = str then 1 else 0) ptn


val q9c_1 = count_some_var("bx", TupleP ([Wildcard, UnitP, ConstructorP ("abc", Wildcard),
                                                     Variable "ax", Variable "bx"]))

fun check_pat (ptn) =
  let
    fun all_vname (ptn, acc) =
      case ptn of
        Variable x => x::acc
      | TupleP ps => List.foldl (fn (x, y) => all_vname(x, []) @ y) acc ps 
      | ConstructorP (_, p) => all_vname(p, []) @ acc 
      | _ => acc
    fun is_unique (vname_list) = 
      case vname_list of
        [] => true
      | x::vname_list' => not (List.exists (fn (y) => x = y) vname_list') andalso is_unique(vname_list')
  in
    is_unique(all_vname(ptn, []))
  end

val q10_1 = check_pat(TupleP ([Wildcard, UnitP, ConstructorP ("abc", Wildcard),
                               Variable "ax", Variable "bx"]))
val q10_2 = check_pat(TupleP ([Wildcard, UnitP, ConstructorP ("abc", Wildcard),
                               Variable "ax", Variable "ax"]))
val q10_3 = check_pat(TupleP ([ConstructorP ("abc", Wildcard), Variable "ax", Variable "bx", TupleP ([Variable "ax"])]))
val q10_4 = check_pat(TupleP ([ConstructorP ("abc", Wildcard), Variable "ax", Variable "bx", TupleP ([Variable "cx", ConstructorP ("axx", Variable "ax")])]))

fun match (valu, ptn) =
  case ptn of
    Wildcard => SOME []
  | UnitP => (case valu of
                Unit => SOME []
              | _ => NONE)
  | ConstP x => (case valu of
                Const y => SOME []
              | _ => NONE)
  | Variable s => SOME [(s, valu)]
  | TupleP ps => (case valu of
                  Tuple vs => (if List.length ps = List.length vs
                              then let val all = all_answers match (ListPair.zip (vs, ps)) 
                                   in case all of
                                        NONE => NONE
                                      | SOME v => SOME (List.foldl (fn (x, y) => x@y) [] v)
                                   end 
                              else NONE)
                | _ => NONE)
  | ConstructorP (s1, p) => (case valu of
                              Constructor (s2, v) => if s1 = s2 then match(v, p) else NONE
                            | _ => NONE)

val q11_a = match (Unit, Wildcard)
val q11_b = match (Unit, Variable "x")

fun first_match valu ptns =
  SOME (first_answer (fn (ptn) => match(valu, ptn)) ptns) handle NoAnswer => NONE

val q11_a = match (Unit, Wildcard)
