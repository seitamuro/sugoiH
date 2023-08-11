data TrafficLight = Red | Yellow | Blue

-- EqのインスタンスとしてTrafficLightを実装
-- Eqの定義に合わせて型を比較できるようにする
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- ShowのインスタンスとしてTrafficLightを実装
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Blue = "Blue light"

-- :info YourTypeClassでYourTypeClassが定義している関数やそれに属する型のリストを表示することができる。