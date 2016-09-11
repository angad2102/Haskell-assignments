import Test.QuickCheck

-- Problem 1: Part I
-- Implementing the Euclidean algorithm to calculate the Greatest Common Divisor of two natural number a and b
gcd' :: Int -> Int -> Int
gcd' a b = if b==0 then a 
 else gcd' b c 
  where 
   c = a `mod` b 

-- Problem 1: Part II
-- Implementing a quickCheck property for our function of Greatest Common Divsor against the library function 
prop_gcd :: Int -> Int -> Property
prop_gcd a b = (a >= 0 && b >= 0) ==> gcd a b == gcd' a b


-- Problem 2: Part I
-- Insering a number in a sorted list 
insert :: Int -> [Int] -> [Int]
insert x [] = [x] 
insert x (y:ys) = if x<y then x : y : ys else y : insert x ys 

-- Problem 2: Part II
-- Insertion Sort 
insertsort :: [Int] -> [Int]
insertsort [x] = [x]
insertsort (x:xs) = insert x (insertsort xs) 

-- Problem 3
-- Implementation of e sieve of Eratosthenes Algorithm to find prime numbers upto a given number
sieve :: [Int] -> [Int]
sieve [x] = [x]
sieve (x:xs) = x : sieve (updatelist x xs)

-- Declaration of an Auxiliary function to return a list after removing all the multiples of x
updatelist :: Int -> [Int] -> [Int]
updatelist x [] = []
updatelist x (y:ys) = if y `mod` x == 0 then (updatelist x ys) else (y : (updatelist x ys)) 

-- The main function which will be called from outside
sievePrime :: Int -> [Int]
sievePrime x = sieve [2..x]

-- Problem 4: Part I
-- Following four prolems are in connection to Collatz conjecture (3n+1 conjecture)
-- One step of Process
next :: Int -> Int
next 1 = 1
next x = if x `mod` 2 == 0 then x `div` 2 else 3 * x + 1

-- Problem 4: Part II
-- Check if Collatz conjecture holds true
terminate :: Int -> Bool
terminate 1 = True
terminate x = terminate (next x)

-- Problem 4: Part III
-- Putting a limit of 500 iterations on terminate
terminate' :: Int -> Int -> Bool
terminate' x i 
 | x == 1 = True
 | i > 500 = False
 | otherwise = (terminate' (next x) (i+1))

-- Problem 4: Part IV
-- QuickCheck property to check the property for n<=10000 with steps >500
prop_terminate :: Int -> Property
prop_terminate n = (n>0 && n<=10000) ==> terminate' n 0

-- Problem 4: Part V
-- number of steps [1..n] take to reach 1
numSteps :: Int -> [Int]
numSteps n = zipWith (\n i -> terminate'' n i) [1..n] (map (\x -> x * 0) [1..n])
  where terminate'' n i = if n ==1 then i else (terminate'' (next n) (i+1))
 