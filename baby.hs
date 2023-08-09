-- 基本的な関数定義やリスト、タプルの扱い方
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

-- 型
addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

-- 型クラス
allSame :: (Eq a) => [a] -> Bool
allSame xs = length [x | x <- xs, x == head xs] == length xs


-- 型注釈
readStringAsInt = read "5" :: Int
readStringAsListInt = read "[5, 4, 3, 2, 1]" :: [Int]