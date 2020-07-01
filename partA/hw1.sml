(* homework 1 *)

(* 1.function is_older that takes two dates and evaluates to true or false *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    ((#1 date1)*10000 + (#2 date1)*100 + (#3 date1))
    < ((#1 date2)*10000 + (#2 date2)*100 + (#3 date2))

(* 2.function number_in_month that returns how many dates *)
fun number_in_month (dates : (int*int*int) list, m : int) =
    if null dates
    then 0
    else if #2 (hd dates) = m
    then 1 + number_in_month(tl dates, m)
    else number_in_month(tl dates, m)
                        
fun number_in_months (dates : (int*int*int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month(dates, hd ms) + number_in_months(dates, tl ms)

fun dates_in_month (dates : (int*int*int) list, m : int) =
    if null dates
    then []
    else
        if #2 (hd dates) = m
        then hd dates :: dates_in_month(tl dates, m)
        else dates_in_month(tl dates, m)

fun dates_in_months (dates : (int*int*int) list, ms : int list) =
    if null ms
    then []
    else dates_in_month(dates, hd ms) @ dates_in_months(dates, tl ms)

fun get_nth (slist : string list, n : int) =
    if n = 1
    then hd slist
    else get_nth(tl slist, n-1)

fun date_to_string (date1 : int*int*int) =
    let
        val months = ["January", "February", "March", "April", "May",
              "June", "July", "August", "September",
              "October", "November", "December"];
    in
        get_nth(months, #2 date1)^" "^Int.toString(#3 date1)^", "^Int.toString(#1 date1)
    end                     

fun number_before_reaching_sum (sum : int, ns : int list) =
    if sum <= hd ns
    then 0
    else 1 + number_before_reaching_sum(sum-(hd ns), tl ns)

fun what_month (days : int) =
    let
        val mdays=[31,28,31,30,31,30,31,31,30,31,30,31];
    in
        1 + number_before_reaching_sum(days, mdays)
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let fun oldest_noempty (dates : (int*int*int) list) =
                 if null (tl dates)
                 then hd dates
                 else
                     let val tl_ans = oldest_noempty(tl dates)
                      in
                          if is_older(hd dates, tl_ans)
                          then hd dates
                          else tl_ans
                     end
        in
            SOME (oldest_noempty(dates))
        end

        
