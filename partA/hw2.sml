
(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(n1 , ns) = 
    case ns of
        [] => NONE
       |n::ns' => if same_string(n1,n)
                  then SOME ns'
                  else case all_except_option(n1, ns') of
                           NONE => NONE
                          |SOME  s => SOME(n::s)

fun get_substitutions1(sll , s) =
    case sll of
        [] => []
       |sl::sl' => let val gets1 = get_substitutions1(sl',s)
                   in
                       case all_except_option(s, sl) of 
                           NONE => gets1
                         | SOME ssl => ssl@gets1
                   end

fun get_substitutions2(sll , s) =
    let fun aux(sll,s,acc) =
               case sll of
                   [] => acc
                  |sl::sl' => case all_except_option(s, sl) of 
                                  NONE => aux(sl',s,acc)
                                | SOME ssl => aux(sl',s,ssl@acc)
    in
        aux(sll,s,[])
    end

fun similar_names (sll, name)=
    let val {first=x, middle=y,last=z} = name
        fun create_names sl =
            case sl of
                [] => []
              | s::sl' => {first=s,middle=y,last=z}::(create_names(sl'))
    in
        name::create_names(get_substitutions2(sll,x))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color c =
    case c of
        (Spades,_) => Black
       |(Clubs,_) => Black
       |(Diamonds,_) => Red
       |(Hearts,_) => Red

fun card_value c =
    case c of
        (_,Ace) => 11
      | (_,Num n) => n
      | (_,_) => 10 

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | s::cs' => if s=c
                  then cs'
                  else remove_card(cs', c, e)

fun all_same_color cs =
    case cs of
        [] => true
      | c::[] => true
      | c1::(c2::c3) => (card_color c1 = card_color c2) andalso all_same_color(c2::c3)
     
fun sum_cards cs =
    let fun aux(cs,acc) =
            case cs of
                [] => acc
              | c::cs' => aux(cs', acc + card_value c)
    in
        aux(cs,0)
    end

fun score (cs, g) =
    let
        val sums = sum_cards cs
        val  pscore =  if sums > g
                       then 3*(sums - g)
                       else g - sums
    in
        if all_same_color cs
        then pscore  div 2
        else pscore
    end

fun officiate (cs, mvs, g) =
    let fun current_state state =
            case state of
                (_,[],hcards) => score(hcards,g)
              | ([],Draw::ms',hcards) => score(hcards,g)
              | (c::cs',Draw::ms',hcards) => if sum_cards(c::hcards) > g
                                             then score(c::hcards,g)
                                             else current_state(cs',ms',c::hcards)
              | (cs,Discard c::cs',hcards) => current_state(cs, cs',remove_card(hcards,c,IllegalMove))
    in
        current_state(cs, mvs, [])
    end
