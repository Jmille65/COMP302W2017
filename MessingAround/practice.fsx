//These are meant to be evaluated separately

//inner x masks x=1- evals to 4
let x = 1 in
  let x = 2 in
    x + x

let x = 1 in
  let y = x in //binds y to 1
    let x = 2 in //binds x to 2
      x + y //2 + 1

//bad binding- x is unbound
let y = x in
  let x = 2 in
    x + y


let x = 1 in
  let f =
    (let u = 3 in (fun y -> u + y + x) ) in
  let x = 2 in
    f(x)

let result = // evals to 7- env diagram practice
  let x = 3 in
  let f =
    let x = x + 1 in
    fun y -> x+y
  f x

// attempt 1
//let bits n =
//  let rec helper n list =
//    match n with
//      | n when n = 1 -> list
//      | n when n > 1 ->
//          let x::xs = list
//          let y = 0::x
//          let z = 1::x
//          helper (n-1) (y::z::(helper n xs))
//      | n when n < 1 -> failwith "invalid number"
//  helper  n [[0];[1]]

//attempt 2
//let bits n =
//  let rec helper n list =
//    match n with
//      | 0 -> [[]]
//      | n when n > 0 ->
//          helper (n-1) (List.fold (fun (x::xs) list -> (0::x)::(1::x)::list) list list)
//  helper n []

//code from someone smarter than me
let rec bits n =
  match n with
    | 0 -> [[]]
    | x ->
      let reList = bits(n-1)
      (List.map (fun u -> 0::u) reList)@(List.map (fun y -> 1::y) reList)
let a = bits 3

let tripleMinusOne f =
  fun x -> ((f (f (f x)))-1)

let multByTwo n =
  n * 2

let dMBT = triple multByTwo
let b = dMBT 2

let rec repeat f n =
  if n = 0 then
    fun x -> x
  else
    fun x -> f(repeat f (n-1) x)

let rMBT = repeat multByTwo 3
let c = rMBT 2
