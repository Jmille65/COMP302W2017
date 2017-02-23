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
