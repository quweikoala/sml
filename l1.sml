fun is_older (d1: int*int*int, d2: int*int*int) =
  if #1 d1 = #1 d2
  then
    if #2 d1 = #2 d2
    then
      #3 d1 < #3 d2
    else #2 d1 < #2 d2
  else #1 d1 < #1 d2

val q1 = is_older((2021, 2, 3), (2021, 1, 3))

fun number_in_month(ds: (int*int*int) list, target_month: int) =
  if null ds
  then 0
  else
    if #2 (hd ds) = target_month
    then 1 + number_in_month(tl ds, target_month)
    else number_in_month(tl ds, target_month)

val q2 = number_in_month([(1,2,3),(2,2,3),(2,3,2)], 2)
      
fun number_in_months(ds: (int*int*int) list, target_months: int list) = 
  if null target_months
  then 0
  else
    number_in_month(ds, hd target_months) + number_in_months(ds, tl target_months)

val q3 = number_in_months([(1,2,3),(2,2,3),(2,3,2),(2,5,1)], [2,5])

fun dates_in_month(ds: (int*int*int) list, target_month: int) =
  if null ds
  then []
  else
    if #2 (hd ds) = target_month
    then (hd ds)::(dates_in_month(tl ds, target_month))
    else dates_in_month(tl ds, target_month)

val q4 = number_in_month([(1,2,3),(2,2,3),(2,3,2)], 2)

fun dates_in_months(ds: (int*int*int) list, target_months: int list) = 
  if null target_months
  then []
  else
    dates_in_month(ds, hd target_months) @ dates_in_months(ds, tl target_months)

val q5 = dates_in_months([(1,2,3),(2,2,3),(2,3,2),(2,5,1)], [2,5])

fun get_nth(strs: string list, n: int) =
  if n > 1
  then get_nth(tl strs, n-1)
  else hd strs

val q6 = get_nth(["abd", "eeee", "ffff", "gggg"], 3)

fun date_to_string(d: int*int*int) = 
  let val month_str = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(month_str, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

val q7 = date_to_string((2021,10,3))

fun number_before_reaching_sum(sum: int, ilist: int list) =
  let
    fun local_reach(cur_sum: int, ilist: int list) =
      if cur_sum + hd ilist >= sum
      then 0
      else 1 + local_reach(cur_sum + hd ilist, tl ilist)
  in
    local_reach(0, ilist)
  end

val q8 = number_before_reaching_sum(10, [1,2,3,4,5,6])
    
fun what_month(day: int) = 
  let
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days) + 1
  end

val q91 = what_month(128)
val q92 = what_month(31)
val q93 = what_month(32)
val q94 = what_month(365)

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)
  
val q10 = month_range(59, 87)

fun oldest(ds: (int * int * int) list) =
  if null ds
  then NONE
  else let
          fun oldest_nonempty(ds: (int * int * int) list) =
            if null (tl ds)
            then hd ds
            else let val tl_ans = oldest_nonempty(tl ds)
                 in
                    if is_older(hd ds, tl_ans)
                    then hd ds
                    else tl_ans
                 end
       in
        SOME(oldest_nonempty ds)
       end

val q11a = oldest([(5,2,3), (2,1,4), (2,1,1)])
val q11b = oldest([])
