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
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = [1] ++ [pascal' n k | k<-[1..n]]

pascal' :: Int -> Int -> Int
pascal' n 0 = 1
pascal' 0 k = 0
pascal' n k = pascal' (n-1) (k-1) * n `div` k
