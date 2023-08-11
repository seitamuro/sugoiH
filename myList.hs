infixr 5 :-:
data MyList a = Empty | a :-: (MyList a) deriving (Show, Read, Eq, Ord)
data MyList' a = Empty' | Cons' { element :: a, next :: MyList' a} deriving (Show, Read, Eq, Ord)