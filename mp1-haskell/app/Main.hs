module Main where

-- mytake
-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake n [] = []
mytake n (x:xs)
  | n < 0 = []
  | otherwise = x : (mytake (n-1) xs)

-- mydrop
-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop 0 xx = xx
mydrop n [] = []
mydrop n (x:xs)
  | n < 0 = (x:xs)
  | otherwise = (mydrop (n-1) xs)
-- rev
-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

-- app
-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app xx yy = xx ++ yy

-- inclist
-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

-- sumlist
-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

-- myzip
-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys
myzip _ _ = []

-- addpairs
-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs (x:xs) (y:ys) = fst(head (myzip (x:xs) (y:ys))) + snd(head (myzip (x:xs) (y:ys))) : addpairs xs ys
addpairs _ _ = []

-- ones
-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

-- nats
-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

-- fib
-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : (addpairs fib (tail fib))

-- add
-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) 
  | x < y = x:y:ys
  | x == y = y:ys
  | otherwise = y:(add x ys)

-- union
-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
  | x == y = x : (union xs ys)
  | x < y = x : (union xs (y:ys))
  | x > y = y : (union (x:xs) ys)

-- intersect
-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
  | x == y = x : (intersect xs ys)
  | x < y = (intersect xs (y:ys))
  | otherwise = intersect (x:xs) ys

-- powerset
-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union xss (map (x:) xss)
                  where xss = powerset xs
                  
-- inclist'
-- don't forget to put the type declaration or you will lose points!
inclist' :: (Num a) => [a] -> [a]
inclist' xs = map (+1) xs

-- sumlist'
-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' xs = foldr (+) 0 xs

data List a = Cons a (List a)
        | Nil
    deriving (Show, Eq)

data Exp = IntExp Integer
        | PlusExp [Exp]
        | MultExp [Exp]
    deriving (Show, Eq)

-- list2cons
-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = (Cons x (list2cons xs))

-- cons2list
-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons n x) = n : cons2list x

-- eval
-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n) = n
eval (PlusExp is) = sum (map eval is)
eval (MultExp is) = foldr (*) 1 (map eval is)

-- list2cons'
-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' xs = foldr Cons Nil xs

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
        | Leaf
    deriving (Show, Eq)

-- sumTree
-- don't forget to put the type declaration or you will lose points!
-- credit to thuang30
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node x y z) = x + sumTree y + sumTree z

-- SimpVal
data SimpVal = IntVal Integer
        | BoolVal Bool
        | StrVal String
        | ExnVal String
        | Nope
    deriving (Show, Eq)

-- liftIntOp
-- don't forget to put the type declaration or you will lose points!
-- credit to thuang30
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal x) (IntVal y) =  IntVal (f x y)
liftIntOp _ _ _ = ExnVal "not an IntVal!" 