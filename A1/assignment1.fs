(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jordan Miller, Id Number: 260513815 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

//module Comp302A1

let rec sumlist l =
  match l with
    | [] -> 0.0
    | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)

let w_mean weights data =
  let testFunc = pairlists (weights, data) //also checks failure conditions
  let rec multiplier a =
    match a with
      | [] -> []
      | (x,y)::xs -> x*y::multiplier xs
  sumlist (multiplier testFunc) / sumlist weights

(* test calls*)
//let data = [3.0;4.0;3.0;5.0]
//let weights = [1.0;2.0;2.5;1.5]
//let c = sumlist data
//let d = pairlists (weights,data)
//let weight = w_mean weights data

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    | (x, []) -> false
    | (x, y::ys) ->
      if x = y then
        true
      else
        memberof (x,ys)

let rec remove(item,lst) =
  match (item,lst) with
    | (x, []) -> []
    | (x, y::ys) ->
      if x = y then
        remove(x, ys)
      else
        y::remove(x, ys)

(*test calls*)
//memberof (6,[2;4;2;1;5;6])
//remove (3,[1;2;3;4;5;3])

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let rec helper(l,m) =
    match (l,m) with
      | ([],y) -> y
      | (x::xs,y) ->
        if x > y then
          helper (xs,x)
        else
          helper (xs,y)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

//findMax [1;2;3;4;5;2;3;10]

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
  match l with
    | [] -> []
    | xs ->
      findMax xs::(selsort (remove (findMax xs, xs)))

//selsort [3;2;5;1;3;7;6]

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | ([],[]) -> failwith "Error -- empty list"
    | (xs,[]) -> failwith "Error -- empty list"
    | ([],ys) -> []
    | (x::xs,ys) ->
      if memberof (x,ys) then
        x::common (remove(x,xs),ys)
      else
        common (remove(x,xs),ys)

//common ([3;4;5;7;2],[1;3;5;7;9;1])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  let rec helper (first, slow, fast) =
    match (first, slow, fast) with
      | (first, ys, ([] | [_])) -> (first, ys)
      | (_, [], _) -> ([],[])
      | (first, x::xs, y::y'::ys) -> helper (x::first, xs, ys)
  helper ([], l, l)

//split [1;2;3]

let rec merge twolists =
  match twolists with
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (x::xs, y::ys) ->
      if x > y then
        y::merge (x::xs, ys)
      else
        x::merge (xs, y::ys)

//merge ([1],[2])

let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | ns ->
    let (xs, ys) = split ns
    merge (mergesort xs, mergesort ys)

//mergesort [3;2;1;8;5;3]
