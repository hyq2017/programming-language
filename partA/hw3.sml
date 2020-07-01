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

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals sl =
    List.filter (fn x => Char.isUpper(String.sub (x,0))) sl

(* 2 *)
fun longest_string1 sl =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" sl

(* 3 *)
fun longest_string2 sl =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" sl

(* 4 *)
fun longest_string_helper f sl =
    foldl (fn (x,y) => if f (String.size x, String.size y) then x else y) "" sl

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
                                            
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
fun rev_string s = (String.implode o List.rev o String.explode) s

(* 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of
                      NONE => first_answer f xs'
                    | SOME x => x

(* 8 *)
fun all_answers f xs =
    let fun aux (xs,acc)=
            case xs of
                [] => SOME acc
              | x::xs' => case f x of
                              NONE => NONE
                            | SOME x => aux(xs', x@acc)
    in
        aux(xs, [])
    end

(* 9a *)
fun count_wildcards p = g (fn () => 1)  (fn x => 0) p
(* 9b *)
fun count_wild_and_variable_lengths p =
    count_wildcards p + (g (fn () => 0) (fn x => String.size x) p)
(* 9c *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if s=x then 1 else 0) p

(* 10 *)
fun check_pat p =
    let
        fun create_vlist p =
            case p of
                Variable x => [x]
               |TupleP  ps => List.foldl (fn (p,l) => (create_vlist p)@l) [] ps
               |ConstructorP(_,p) => create_vlist p
               |_ => []
        fun check_list sl =
            case sl of
                [] => true
               |x::xs' => not (List.exists (fn y => x=y) xs') andalso check_list xs'
    in
        (check_list o create_vlist) p
    end

(* 11 *)
fun match vp =
    case vp of
        (_,Wildcard) => SOME []
       |(v, Variable s) => SOME [(s,v)]
       |(Unit, UnitP) => SOME []
       |(Const i, ConstP j) => if i=j then SOME [] else NONE
       |(Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers (fn vp => match vp) (ListPair.zip (vs, ps))
                                 else NONE
       |(Constructor(s2,v), ConstructorP (s1,p)) => if s2=s1 
                                                    then match (v,p)
                                                    else NONE
       |_ => NONE

(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn x => match (v, x)) ps)
    handle NoAnswer => NONE
