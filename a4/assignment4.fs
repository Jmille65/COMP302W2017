(* Name: Jordan Miller, Student ID: 260513815*)

type id = string

type term =
  | Var of id
  | Const of int
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool =
  match t with
    | Var(termToCheck) -> termToCheck = x
    | Const(termToCheck) -> false
    | Term(_, termListToCheck) -> List.exists (occurs x) termListToCheck

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : term) (x : id) (t : term) : term =
  match t with
    | Var(termToReplace) ->
        if termToReplace = x then s
        else t
    | Const(termToCheck) -> Const(termToCheck)
    | Term(funSym, termListToReplace) ->
        Term(funSym, List.map (subst s x) termListToReplace)

(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term) : term =
  List.foldBack (fun (oldVar, newTerm) -> subst newTerm oldVar) s t

let testSub = [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
let testTerm = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])

//apply testSub testTerm

(* unify one pair *)

let rec unify (s : term) (t : term) : substitution =
  let rec matchPairs l1 l2 =
    match (l1, l2) with // other cases won't happen based on design
      | (x::xs, y::ys) -> (x,y)::matchPairs xs ys
      | ([],[]) -> []

  match (s, t) with
    | (Const constOne, Const constTwo) ->
        if constOne = constTwo then []
        else failwith "not unifiable: clashing constants"
    | ((Const constOne, Var idOne) | (Var idOne, Const constOne)) ->
        [(idOne, Const constOne)]
    | (Const constOne, _) ->
        failwith "not unifiable: term constant clash"
    | (_, Const constTwo) ->
        failwith "not unifiable: term constant clash"
    | (Var idOne, Var idTwo) ->
        if idOne = idTwo then []
        else [(idOne, t)]
    | ((Var idOne, (Term(_) as t)) | ((Term(_) as t), Var idOne)) ->
        if occurs idOne t then failwith "not unifiable: circularity"
        else [(idOne, t)]
    | (Term (funcSymOne, varsOne), Term (funcSymTwo, varsTwo)) ->
        if (List.length varsOne) = (List.length varsTwo) && funcSymOne = funcSymTwo then
          unify_list (matchPairs varsOne varsTwo)
        else failwith "not unifiable: head symbol conflict"

(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution =
  match s with
    | [] -> []
    | (t1, t2) :: termList ->
        let substList = unify_list termList
        let substListTwo = unify (apply substList t1) (apply substList t2)
        substList @ substListTwo

let secondTerm = Term("f", [Term ("k", [Term("h", [Var "z"])]); Term ("h", [Var "z"]); Term ("k", [Term ("h", [Var "z"])])])
//unify testTerm secondTerm

let testTermTwo = Term("x", [Var "c"; Var "e"])
let otherTestTerm = Term("x", [Var "d"; Const 3])
//unify testTermTwo otherTestTerm
(*
Examples
> let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
val t1 : term = Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])
> let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
val t2 : term =
  Term ("f",[Term ("g",[Var "z"]); Term ("h",[Var "x"]); Var "y"])
> let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
val t3 : term = Term ("f",[Var "x"; Var "y"; Term ("g",[Var "u"])])
> unify t1 t2;;
val it : substitution =
  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))]
> let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
val t4 : term = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])
>  let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
val t5 : term = Term ("f",[Term ("k",[Var "y"]); Var "y"; Var "x"])
> unify t4 t5;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> unify t5 t4;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> apply it t4;;
val it : term =
  Term
    ("f",
     [Term ("k",[Term ("h",[Var "z"])]); Term ("h",[Var "z"]);
      Term ("k",[Term ("h",[Var "z"])])])
> let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
val t6 : term = Term ("f",[Const 2; Var "x"; Const 3])
> let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
val t7 : term = Term ("f",[Const 2; Const 3; Var "y"])
> unify t6 t7;;
val it : substitution = [("x", Const 3); ("y", Const 3)]
> apply it t7;;
val it : term = Term ("f",[Const 2; Const 3; Const 3])
> unify t1 t7;;
System.Exception: not unifiable: term constant clash
....... junk removed .............
Stopped due to error
*)
