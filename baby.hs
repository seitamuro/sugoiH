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

-- パターンマッチ
lucky :: Int -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = factorial (x-1) * x

addVector :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVector x y = (fst x + fst y, snd x + snd y)

-- リストのパターンマッチ
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

head'' :: [a] -> a
head'' (_:x:_) = x
head'' xs = error "Can't call head on an empty list, dummy!"

iThinkThisList :: (Show a) => [a] -> String
iThinkThisList [] = "The list is empty"
iThinkThisList (x:[]) = "The list has one element: " ++ show x
iThinkThisList (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
iThinkThisList (x:y:_) = "The List has many elements!"

firstLetter :: String -> String
firstLetter "" = error "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- ガード
bmiTell :: Float -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell' :: Float -> Float -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a < b = LT
  | a > b = GT
  | a == b = EQ

a `myCompare` b
  | a < b = LT
  | a > b = GT
  | a == b = EQ

-- where
bmiTell'' :: Float -> Float -> String
bmiTell'' weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
  where
    (f:_) = firstName
    (l:_) = lastName

bmis :: [(Float, Float)] -> [Float]
bmis wh = [bmi w h | (w, h) <- wh]
  where
    bmi w h = w / h^2

-- let
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * r * pi * h
      topArea = pi * r^2
  in sideArea + topArea * 2

bmis' :: [(Float, Float)] -> [Float]
bmis' xs = let bmi w h = w / h^2 in [bmi w h | (w, h) <- xs]

bmis'' :: [(Float, Float)] -> [Float]
bmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- case
head''' :: [a] -> a
head''' xs = case xs of (x:_) -> x
                        [] -> error "No head for empty list!"

describeList :: (Show a) => [a] -> String
describeList xs = "This list is " ++ case xs of (x:[]) -> "is single list of [" ++ show x ++ "]"
                                                (x:y:[]) -> "is double list of [" ++ show x ++ ", " ++ show y ++ "]"
                                                [] -> "empty!"
                                                (x:y:_) -> "many!"