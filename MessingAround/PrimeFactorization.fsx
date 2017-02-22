let rec f x n a = //x is given num, n is potential factor, a is list
  if n*n > x then
    x::a
  elif x % n = 0 then
    f (x/n) n (n::a)
  else
    f x (n+1) a
let factorise n = f n 2 []

let factors = factorise 7340 //calls
