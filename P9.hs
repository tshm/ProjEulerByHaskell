module P9 where
pythagoreanTriplet = [(a,b,c) |
  a <- [1..999], b <- [1..999],
  let c = 1000 - b - a,
  a^2 + b^2 == c^2]

ans = let (a,b,c) = head pythagoreanTriplet in a * b * c

main = do
  print ans

