let pair x y = \z -> z x y

-- Define the horrendous value using nested lets
let horrendous =
  let x1 = \y -> pair y y in
  let x2 = \y -> x1 (x1 y) in
  let x3 = \y -> x2 (x2 y) in
  let x4 = \y -> x3 (x3 y) in
  -- Apply x4 to the identity function
  x4 (\z -> z)