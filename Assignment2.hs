import Test.QuickCheck
import Data.Char
import Data.List (union)
import Data.Maybe (fromMaybe)
import Data.List
import Control.Monad
import Control.Applicative hiding (many)

-- Problem 1: Part I
-- Gives a list of numbers which are in between the 2 arguments specified. This function takes care of negative numbers as well.
range :: Int -> Int -> [Int]
range x y | x<=y = take (abs(y-x)+1) (iterate (+1) x) 
          | otherwise = []

-- Problem 1: Part II
--Quick Check property to check the upper function : Test cases passed - 100 % 
prop_range :: Int -> Int -> Bool
prop_range x y = (enumFromTo x y) == (range x y)

--Problem 2: Part I
-- don't use this inefficient definition using fact:
--bi_coeff n k = fact n `div` (fact k * fact (n - k))
-- Binomial Coefficient using recursion. No factorials used.  
bi_coeff :: Int -> Int -> Int
bi_coeff n 0 = 0
bi_coeff n k | (k>n) = 0 
             | (n==k) = 1
             | otherwise = n*(bi_coeff (n-1) k)`div`(n-k)

--Problem 2: Part II
-- Determine all the possible combinations of a given list and number of elements in the combination
comb :: Eq a => Int -> [a] -> [[a]]
comb 0 _ = []
comb k [] = []
comb 1 xs = [ [x] | x <- xs ] 
comb k xs | (k > (length xs)) = []
          | otherwise = [ [ xs !! i ]++y | i <- [0..(length xs)-1], y <- comb (k-1) (drop (i+1) xs) ]

--Problem 2: Part III
--Checks if the number of results provided by comb is equal to bi_coeff : Test cases passed - 100%
prop_comb :: Eq a => Int -> [a] -> Property
prop_comb k xs = (k >=0 && length xs <= 20) ==> length (comb k xs) == bi_coeff (length xs) k

--Problem 3
--Checks for a valid identifier (Cannot start with a hyphen, cannot have two consecutive hyphens, does not end with a hyphen, starts with a letter and only consists of Letters, numbers and hyphens)
identifier :: String -> Bool
identifier [] = False
identifier xs = and [x | i <- [0..(length xs)-1], z <- [(xs !! i)], x <- [a i z] ]
 where a i z| ((i==0) && (not (elem z ['a'..'z']))) && (not (elem z ['A'..'Z'])) = False
            | ((not (elem z ['a'..'z'])) && (not (elem z ['A'..'Z']))) && ((not (elem z ['0'..'9'])) && (not (z=='-'))) = False
            | ((z=='-') && (not (i==(length xs)-1)))&& ((xs !! (i+1)) == '-') = False
            | (i==(length xs)-1) && (z=='-') = False
            | otherwise = True

-- Problem 4: Part I
-- Definition for Data Tree
data Tree a = Leaf | Node Int (Tree a) a (Tree a)
              deriving (Eq, Show)

--Calculates the height of a given tree 			  
height :: Tree a -> Int
height Leaf = 0
height (Node n a _ c) = if (height a > height c) then 1 + height a else 1 + height c

--toTree function using foldr
toTree :: [a] -> Tree a
toTree = foldr (\x tree -> insertxTree x tree) Leaf

--insertxTree function definition here. This keeps in check that the tree being created is balanced and inserts Nodes at the correct locations. 
insertxTree :: a -> Tree a -> Tree a
insertxTree x Leaf = Node 1 (Leaf) x (Leaf)
insertxTree x (Node n t1 val t2) 
    | h1 < h2   = Node  n t1n val t2 
    | h1 > h2   = Node  n t1  val t2n
    | nh1 < nh2 = Node  n t1n val t2
    | otherwise = Node (nh2+1) t1  val t2n 
  where h1  = height t1
        h2  = height t2
        t1n = insertxTree x t1
        t2n = insertxTree x t2
        nh1 = height t1n
        nh2 = height t2n

-- Problem 4: Part II
--QuickCheck property defined to Check if the tree which results from the previous function is indeed balanced or not
prop_toTree :: [a] -> Bool
prop_toTree xs = and (check (toTree xs))

-- Helper function for QuickCheck. Rather than using the height in the Tree data, this function actually checks for a balanced tree using recursion
check :: Tree a -> [Bool] 
check Leaf = [True]
check (Node n a x b) | abs(height a - height b)<=1 = [True] ++ (check a) ++ (check b)
                   | otherwise = [False]  

-- Problem 5: Part I
-- Declaration of Circuit
type Size = Int --positive
data Circuit = Identity Size |
               Fan Size |
               Above Circuit Circuit |
               Beside Circuit Circuit |
               Stretch [Size] Circuit
               deriving (Show)

-- Calculates the width of a given Circuit 			   
width :: Circuit -> Maybe Int
width (Identity n) = Just n
width (Fan n) = Just n
width (Above a b) = if (width a == width b) then width a else Nothing
width (Stretch xs a) = if (Just (length xs) == width a) then Just (sum xs) else Nothing 
width (Beside a b) = calculatebesidewidth (width a) (width b) 

-- A helper function defined to determine the width of (Beside a b)      
calculatebesidewidth :: Maybe Int -> Maybe Int -> Maybe Int
calculatebesidewidth (Just n) (Just m) = Just (n+m)
calculatebesidewidth  _ _ = Nothing    

-- Problem 5: Part II
--This function evaluates the Given circuit and returns an list of type [[Int]]
eval :: Circuit -> [[Int]]
eval (Identity n) = [ [x] | x <- [1..n]]
eval (Fan n) = [[1]] ++ (tail [ x | y <- [1..n], x <-[ [1]++[y] ]])
eval (Beside a b) = (eval a) ++ (addwidth (eval b) (width a))
eval (Stretch (x:xs) y) = indexcorrector (stretchbuilder (x:xs) (eval y) 0) (x:xs) 0
eval (Above a b) = abovereplace (eval a) (eval b)

--addwidth is a helper function to evaluate circuits of type (Beside a b)
addwidth :: [[Int]] -> Maybe Int -> [[Int]]
addwidth xs (Just n) = [ (addn y n )| y <- xs]
addwidth xs (_) = []

--addn is a helper function for addwidth
addn :: [Int] -> Int -> [Int]
addn [] n = []
addn (x:xs) n = (x+n):(addn xs n)

--indexcorrector is a helper function to evaluate circuits of type (Stretch xs y)
indexcorrector :: [[Int]] -> [Int] -> Int -> [[Int]]
indexcorrector [] _ _ = [] 
indexcorrector (y:ys) xs i | (length y)==1 = [i+1]:(indexcorrector ys xs (i+1))
                           | otherwise = (process xs y i):(indexcorrector ys xs (i+1)) 

--process is a helper function for indexcorrector						   
process :: [Int] -> [Int] -> Int -> [Int]
process xs (y:ys) i | ys==[] = [i+1]
                    | otherwise = (processy y xs i):(process xs ys  i)

--processy is a helper function for process
processy :: Int -> [Int] -> Int -> Int
processy y (x:xs) i | xs==[] = (x+y-1)
                    | y>x = (processy y xs i)
                    | otherwise = (x+y-1)

--Stretchbuilder is a helper function to evaluate circuits of type (Stretch xs y)
stretchbuilder :: [Int] -> [[Int]] -> Int -> [[Int]]
stretchbuilder [] _ _ = []
stretchbuilder (x:xs) ys i = (eval (Identity (x-1))) ++ [(ys!!i)] ++ (stretchbuilder xs ys (i+1))   

-- This is a helper function to evaluate circuits of type (Above a b)
abovereplace :: [[Int]] -> [[Int]] -> [[Int]]
abovereplace [] [] = [] 
abovereplace (x:xs) (y:ys) =  [sort (clear ((init x)++y) 0)] ++ (abovereplace xs ys)

--clear is a function used tog et rid of repeated elements in the list given by abovereplace
clear :: [Int] -> Int -> [Int]
clear [] _= []
clear xs i | (length xs) -1 == i = [xs!!i]
           |(length xs) -1 < i+1 = []
           |(xs!!i) == (xs!!(i+1)) = (xs!!i):(clear xs (i+2))
           | otherwise = (xs!!i):(clear xs (i+1))