module Person (
  Person(..)
) where

-- レコード構文
-- 自動的に各フィールドの値を抽出する関数を生成する
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)