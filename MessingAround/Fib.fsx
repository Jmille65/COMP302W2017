let rec fib x =
    if x < 2 then 1
    else fib (x - 2) + fib (x - 1)
printfn "%d" (fib 10)
