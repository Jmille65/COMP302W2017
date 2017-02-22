module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Jordan Miller, Id Number: 260513815 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *)

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

let rec newton(f,guess:float,tol:float,dx:float) =
  if abs (f (guess)) < tol then
    guess
  else
    newton(f, guess - (f(guess))/(deriv(f, dx) guess), tol, dx)


//For testing
//let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)
//newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)
//
//let root = newton(sin,5.0,0.0001,0.0001)

(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly =
  if p.IsEmpty then
    raise EmptyList
  else
    Poly(List.map (fun (x,y) -> (x*c, y+e)) p)

let addTermToPoly(Term (c,e):term, Poly p:poly):poly =
  if p.IsEmpty then raise EmptyList
  let rec helper(Term (c,e):term, x) =
    match x with
      | [] -> [] // this will never be reached
      | (cP, eP)::ps ->
        if eP > e then
          (cP, eP)::helper(Term (c,e), ps)
        else if eP = e then
          (cP+c, eP)::ps
        else
          (c,e)::(cP,eP)::ps
  Poly(helper(Term (c,e), p))

let addPolys(Poly p1:poly, Poly p2:poly):poly =
  if p1.IsEmpty || p2.IsEmpty then raise EmptyList
  let rec helper(p1, p2) =
    match p1 with
      | [] -> Poly(p2)
      | (x,y)::p -> addTermToPoly(Term (x,y), helper(p, p2))
  helper(p1, p2)

let multPolys(Poly p1:poly, Poly p2:poly) =
  List.map(fun (x,y) -> multiplyPolyByTerm(Term(x,y), Poly(p2))) p1
  |> List.fold(fun final instancePoly -> addPolys(final, instancePoly)) (Poly([(0.0,0)]))

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float =
  if p.IsEmpty then raise EmptyList
  List.sumBy(fun(x,y) -> evalTerm v (Term(x,y))) p

let diffPoly (Poly p) =
  Poly(List.map(fun (x, y) -> (x*float(y), y-1)) (List.filter(fun (x,y) -> y<>1) p))

(* Question 3 *)
type Exptree =
  | Const of int
  | Var of string
  | Add of Exptree * Exptree
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

let rec lookup(name:string, env: Bindings) = failwith "Not implemented"

let rec insert(name:string, value: int, b: Bindings) = failwith "Not implemented"

let rec eval(exp : Exptree, env:Bindings) = failwith "Not implemented"

(* For testing

let env:Bindings = [("a",3);("b",4);("c",5)]

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)

*)


(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>

let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

let pointsMade (r: Result) =
  let ((a,b),(c,d)) = r
  if b < d then
    ((a, (Points 0)), (c, (Points 3)))
  else if b > d then
    ((a, (Points 3)), (c, (Points 0)))
  else
    ((a, (Points 1)), (c, (Points 1)))

let initEntry (name:Team) = (name, Points 0)

let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table =
  let ((a,Points b),(c,Points d)) = pointsMade(r)
  let (Points aPoints) = (Map.find a t)
  let secondT = Map.add a (Points(aPoints+b)) t
  let (Points cPoints) = (Map.find c t)
  Map.add c (Points(aPoints+d)) secondT

let rec weekendUpdate(t:Table,rl: Result list): Table =
  match rl with
    | [] -> t
    | x::xs -> weekendUpdate(updateTable(t, x), xs)

let rec seasonUpdate(t:Table, sll:Result list list) : Table =
  match sll with
    | [] -> t
    | x::xs -> weekendUpdate(seasonUpdate(t, xs), x)

let less((s1,n1):Team * Points, (s2,n2):Team * Points) =
  n1 < n2 // lol

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)

let l1 = initializeTable league
let l2 = showStandings(seasonUpdate(l1, s))

(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]

let makeRoadMap data =
  let rec helper data =
    match data with
      | [] -> Map.empty
      | (d, dl)::xs -> Map.add (City d) (Set.ofList(List.map (City) dl)) (helper xs)
  Roads (helper data)

let rec upToManySteps (Roads r) n startCity =
  if n < 0 then
    failwith "Invalid depth"
  else if n = 0 then Set.empty
  else
    let collectorSet = r.TryFind(startCity) // set of child cities
    match collectorSet with
      | Some neighbours ->
        Set.fold (fun neighbours currentNeighbour ->
          (Set.union neighbours (upToManySteps (Roads r) (n-1) currentNeighbour))) neighbours neighbours
          |> Set.add(startCity)
      | None -> Set.empty
