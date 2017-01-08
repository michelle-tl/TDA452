import Test.QuickCheck
import Data.List

---- Exercises 2 ----

-- 1. The Maximum function

-- returns the maximum of two numbers
maxi :: Int -> Int -> Int
maxi x y | x>=y = x
        | otherwise = y

prop_maxi :: Int -> Int -> Bool
prop_maxi x y = maxi x y == max x y



-- 2. Sum of squares

-- returns the sum of the squares of the numbers from 1 to n
sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = (n*n) + sumsq (n-1)

sumsq' :: Int -> Int
sumsq' n = n * (n+1) * (2*n + 1) `div` 6

prop_sumsq :: Int -> Bool
prop_sumsq n = sumsq' n' == sumsq n'
   where
      n' = abs n

-- 4. Fibonacci Numbers

-- computes the nth fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- 5. Factors

-- return the smallest factor of n larger than one
smallestFactor :: Int -> Int
smallestFactor n  | n == 1 = 1
                  | otherwise    = nextFactor 2 n


-- helper function for smallest factor
nextFactor :: Int -> Int -> Int
nextFactor k n
                  | k == n = k
                  | n `mod` k == 0 = k
                  | otherwise = nextFactor (k+1) n

prop_smallestFactorDiv :: Int -> Bool
prop_smallestFactorDiv n = (n `div` res) * res == n
  where
    res = smallestFactor n

prop_smallestFactorMod :: Int -> Bool
prop_smallestFactorMod n = n `mod` smallestFactor n == 0

numFactors :: Int -> Int
numFactors n = numFactors' n n

numFactors' :: Int -> Int -> Int
numFactors' n k  | k == 1 =1
                | n `mod` k == 0 = 1 + numFactors' n(k-1)
                | otherwise = numFactors' n (k-1)


-- 6. Multiplying list elements

data List a = Empty | Add a (List a)

multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * multiply xs


-- 7. Avoiding Duplicates

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) | elem x xs = True
                  | otherwise = duplicates xs

-- 9. Defining Types

data Month = January | February | March | April | May | June | July | August | September | October | November | December
              deriving(Eq, Show)

-- computes th number of days in a month

daysInMonth :: Month -> Int -> Int
daysInMonth month year
                        | month == February && leapyear year = 29
                        | month == February = 28
                        | even month = 31
                        | otherwise = 30
            where
                leapyear year = year `mod`4 == 0
                even month = elem month [April, June, September, November]

data Date = Date{year :: Int, month :: Month, day :: Int}

validateDate :: Date -> Bool
validateDate d = (day d) == daysInMonth (month d) (year d)


---- Exercises 3 ----

-- 0. Defining Functions over Lists
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) | n > 0 = drop' (n-1) xs
               | n <=0 = x:xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' _ [] = ([], [])
splitAt' n xs | n > 0 = (take n xs, drop n xs)

-- 1. Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation [] _      = False
isPermutation (x:xs) ys | x `elem` ys = isPermutation xs (ys \\ [x])
                        | otherwise   = False

-- 2. Avoiding duplicates

duplicates' :: Eq a => [a] -> Bool
duplicates' [] = False
duplicates' (x:xs) | not (elem x xs) = duplicates' xs
                  | otherwise = False

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))


-- 3. Pascal's Triangle

-- computing the nth row in a pascal triangle by recursively calling pascal'
-- pascal' computes "n choose k" where k starts from 1 to n
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = [1] ++ [pascal' n k | k<-[1..n]]

pascal' :: Int -> Int -> Int
pascal' n 0 = 1
pascal' 0 k = 0
pascal' n k = pascal' (n-1) (k-1) * n `div` k

-- 4. Erastosthenes' sieve

-- removes all multiples of m from ns without recursion
crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [x | x<-ns, x `mod` m /= 0 ]

-- returns a list with prime numbers that are found
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = [x]++sieve (filter (\y -> y `mod` x /=0)xs)

-- 5. Number Games

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = True
isPrime n | n > 1 && n<=100 = n `elem` sieve[2..100]

isSumOfTwoPrimes :: Int -> Bool
isSumOfTwoPrimes 0 = False
isSumOfTwoPrimes 1 = False
isSumOfTwoPrimes n = or [ (n - x) `elem` list | x <- list ]
  where
      list = sieve[2..(n-1)]


-- 6. Occurrences in Lists

-- occursIn x xs, which returns True if x is an element of xs.
occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = x `elem` xs

-- allOccurIn xs ys, which returns True if all of the elements of xs are also elements of ys.
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn xs ys = and [x `elem` ys | x<-xs]

-- sameElements xs ys, which returns True if xs and ys have exactly the same elements.
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = length xs == length ys && allOccurIn xs ys

-- numOccurrences x xs, which returns the number of times x occurs in xs.
numOccurences :: Eq a => a -> [a] -> Int
numOccurences x xs = sum [if x == y then 1 else 0 | y <- xs ]

-- convert a list into a bag
bag :: Eq a => [a] -> [(a, Int)]
bag xs = [ (x, numOccurences x xs)| x<-nub xs]


-- 7 Elements and Positions

-- positions xs, which converts a list into a list of pairs of elements and their positions. Hint: Make use of the standard function zip.
positions :: Eq a => [a] -> [(a, Int)]
positions (xs) = zip xs posList
  where
    posList = [1..length xs]

-- firstPosition x xs, which returns the first position at which x occurs in xs.
firstPosition :: Eq a => a -> [a] -> Int
firstPosition x (y:xs) | x==y = 1
                       | x `elem` xs = 1 + firstPosition x xs
                       | otherwise = -1

-- remove1 x xs, which removes the first occurrence of x from xs. For example, remove1 'l' "hello" == "helo"
remove1 :: Eq a => a -> [a] -> [a]
remove1 x xs = remove1' (firstPosition x xs) xs
  where
    remove1' 0 xs = xs
    remove1' 1 (x:xs) = xs
    remove1' n (x:xs) = x : remove1' (n-1) xs

-- remove n x xs, which removes the first n occurrences of x from xs.
remove :: Eq a => Int -> a -> [a] -> [a]
remove n x xs | n>0 = remove (n-1) x $ remove1 x xs
              | otherwise = xs


-- 8 (*). More List Comprehensions

-- returns a list of all combinations of the elements from these two lists
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]

-- A Pythagorean triad is a triple of integers (a,b,c) such that a2 + b2 = c2. Find all Pythagorean triads with a≤b≤c≤100.
pythagoreanTriad :: [(Int,Int,Int)]
pythagoreanTriad = [(a,b,c) | c <- [1..100],
                              b <- [1..c],
                              a <-[1..b],
                              a^2+b^2==c^2]
