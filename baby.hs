doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
rightTriangle (a, b, c) = if a*a+b*b == c || b*b+c*c==a*a || c*c+a*a==b*b then True else False
rightTriangles xs = [x | x <- xs, rightTriangle x]
sumIs24 (a, b, c) = a+b+c == 24
requiredTriangle = [(a, b, c) | a <- [1..10], b <- [1..a], c <- [1..b], sumIs24 (a, b, c), rightTriangle (a, b, c)]