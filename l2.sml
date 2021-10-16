(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (target, xs) =
  let
    fun trace xs = 
      case xs of
        [] => []
      | x::xs' => if same_string(x, target) then trace xs' else x::trace xs'
    val trace_ans = trace(xs)
  in
    if length xs = length trace_ans
    then NONE
    else SOME(trace_ans)
  end

val q1a_1 = all_except_option ("a", ["b", "a", "c"])
val q1a_2 = all_except_option ("a", ["b", "c"])

fun get_substitutions1 (substitutions, s) = 
  case substitutions of
    [] => []
  | x::xs' => case all_except_option(s, x) of
                NONE => get_substitutions1(xs', s)
              | SOME i => i @ get_substitutions1(xs', s)

val q1b_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
val q1b_2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")

fun get_substitutions2 (substitutions, s) = 
  let fun aux(substitutions, acc) = 
    case substitutions of
      [] => acc
    | x::xs' => case all_except_option(s, x) of
                  NONE => aux(xs', acc)
                | SOME i => aux(xs', i @ acc)
  in
    aux(substitutions, [])
  end

val q1c_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
val q1c_2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")

fun similar_names (substitutions, {first=x,middle=y,last=z}) =
  let
    val subs = get_substitutions1(substitutions, x)
    fun help_fun (xs) = 
      case xs of
        [] => []
      | head::xs' => {first=head, middle=y, last=z} :: help_fun(xs')
  in
    {first=x,middle=y,last=z}::help_fun(subs)
  end

val q1d = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (c) =
  case c of
    (Clubs, _) => Black
   | (Diamonds, _) => Red
   | (Hearts, _) => Red
   | (Spades, _) => Black

val q2a_1 = card_color((Clubs, Queen))
val q2a_2 = card_color((Hearts, Num 4))
    
fun card_value (c) = 
  case c of
    (_, Ace) => 11
  | (_, Num i) => i
  | (_, _) => 10

val q2b_1 = card_value((Clubs, Queen))
val q2b_2 = card_value((Hearts, Num 4))
val q2b_3 = card_value((Hearts, Ace))

fun remove_card (cs, c, e) =
  let
    fun aux(cs, found) = 
      case cs of
        [] => []
      | card::cs' => if found then card::aux(cs', true) else if card = c then aux(cs', true) else card::aux(cs', false)
    val removed = aux(cs, false)
  in
    if length removed = length cs
    then raise e
    else removed
  end
  
val q2c_1 = remove_card([(Hearts, Ace), (Clubs, Num 4)], (Hearts, Ace), IllegalMove)
val q2c_4 = remove_card([(Clubs, Num 4), (Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove)
(* val q2c_3 = remove_card([(Hearts, Ace), (Clubs, Num 4)], (Hearts, Queen), IllegalMove) *)
val q2c_5 = remove_card([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace)], (Hearts, Ace), IllegalMove)
val q2c_6 = remove_card([(Hearts, Ace), (Clubs, Num 4), (Hearts, Jack)], (Hearts, Ace), IllegalMove)
val q2c_2 = remove_card([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove)

fun all_same_color (cs) = 
  case cs of
     [] => true
   | c::[] => true
   | c::d::cs' => if card_color(c) = card_color(d) then all_same_color(d::cs') else false

val q2d_5 = all_same_color([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace)])
val q2d_6 = all_same_color([(Hearts, Ace), (Hearts, Num 4), (Hearts, Jack)])

fun sum_cards (cs) =
  let
    fun aux (cs, acc) = 
      case cs of
        [] => acc
      | card::cs' => aux(cs', acc+card_value(card))
  in
    aux(cs, 0)
  end

val q2e_5 = sum_cards([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace)])
val q2e_6 = sum_cards([(Hearts, Ace), (Clubs, Num 4), (Hearts, Jack)])
val q2e_2 = sum_cards([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace), (Hearts, Ace)])

fun score (cs, goal) = 
  let
    val sum_score = sum_cards(cs)
    val preliminary = if sum_score > goal then 3 * (sum_score - goal) else goal - sum_score
  in
    if all_same_color(cs) then preliminary div 2 else preliminary 
  end

val q2f_1 = score([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace)], 10)
val q2f_2 = score([(Hearts, Ace), (Hearts, Num 4), (Hearts, Ace)], 10)
val q2f_3 = score([(Hearts, Ace), (Hearts, Num 4), (Hearts, Ace)], 100)

fun officiate(cs, ml, goal) =
  let
    fun take_move(hands, ml, cs) = 
      case ml of 
        [] => score(hands, goal)
      | move::ml' => case move of
                       Draw => (case cs of
                                  [] => score(hands, goal)
                                | card::cs' => let val after_score = score(card::hands, goal) 
                                               in if after_score > goal then after_score else take_move(card::hands, ml', cs')
                                               end)
                     | Discard c => take_move(remove_card(hands, c, IllegalMove), ml', cs)
  in
    take_move([], ml, cs)
  end


val q2g_1 = officiate([(Hearts, Ace), (Clubs, Num 4), (Hearts, Ace)], [Draw, Draw], 10)
