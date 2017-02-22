let FastFib x =
  let rec loop acc1 acc2 = function
    | n when n = 0 -> acc1
    | n -> loop acc2 (acc1 + acc2) (n - 1)
  loop 0 1 x
FastFib 15
