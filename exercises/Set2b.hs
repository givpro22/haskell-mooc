module Set2b where

import Mooc.Todo

-- Some imports you'll need. Don't add other imports :)
import Data.List

------------------------------------------------------------------------------
-- Ex 1: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

-- [한국어 해석] 이항계수 B(n,k)를 재귀로 계산합니다. 기본식: B(n,0)=1, k>0일 때 B(0,k)=0, 그 외에는 B(n-1,k)+B(n-1,k-1). 경계값(k<0 또는 k>n)은 0으로 처리합니다.
binomial :: Integer -> Integer -> Integer
binomial n k
  | k == 0              = 1
  | n == 0 && k > 0     = 0
  | k < 0 || k > n      = 0
  | otherwise           = binomial (n-1) k + binomial (n-1) (k-1)

------------------------------------------------------------------------------
-- Ex 2: implement the odd factorial function. Odd factorial is like
-- factorial, but it only multiplies odd numbers.
--
-- Examples:
--   oddFactorial 7 ==> 7*5*3*1 ==> 105
--   oddFactorial 6 ==> 5*3*1 ==> 15

-- [한국어 해석] 홀수만 곱하는 팩토리얼입니다. n이 홀수면 n*(n-2)*(n-4)*...*1, 짝수면 (n-1)부터 내려갑니다. 0 이하인 경우 1을 반환합니다.
oddFactorial :: Integer -> Integer
oddFactorial n
  | n <= 0     = 1
  | odd n      = n * oddFactorial (n - 2)
  | otherwise  = oddFactorial (n - 1)

------------------------------------------------------------------------------
-- Ex 3: implement the Euclidean Algorithm for finding the greatest
-- common divisor:
--
-- Given two numbers, a and b,
-- * if one is zero, return the other number
-- * if not, subtract the smaller number from the larger one
-- * replace the larger number with this new number
-- * repeat
--
-- For example,
--   myGcd 9 12 ==> 3
-- In this case, the algorithm proceeds like this
--
--   a      b
--
--   9      12
--   9      (12-9)
--   9      3
--   (9-3)  3
--   6      3
--   (6-3)  3
--   3      3
--   (3-3)  3
--   0      3
--
-- Background reading:
-- * https://en.wikipedia.org/wiki/Euclidean_algorithm

-- [한국어 해석] 유클리드 호제법(뺄셈 버전). 둘 중 하나가 0이면 다른 값을 반환하고, 그렇지 않으면 큰 값에서 작은 값을 뺀 뒤 반복합니다.
myGcd :: Integer -> Integer -> Integer
myGcd a b
  | a == 0    = b
  | b == 0    = a
  | a > b     = myGcd (a - b) b
  | otherwise = myGcd a (b - a)

------------------------------------------------------------------------------
-- Ex 4: Implement the function leftpad which adds space characters
-- to the start of the string until it is long enough.
--
-- Examples:
--   leftpad "foo" 5 ==> "  foo"
--   leftpad "13" 3 ==> " 13"
--   leftpad "xxxxx" 3 ==> "xxxxx"
--
-- Tips:
-- * you can combine strings with the ++ operator.
-- * you can compute the length of a string with the length function

-- [한국어 해석] 문자열 앞에 공백을 붙여 길이가 n이 될 때까지 채웁니다. 이미 길이가 n 이상이면 그대로 반환합니다.
leftpad :: String -> Int -> String
leftpad s n
  | len >= n  = s
  | otherwise = replicate (n - len) ' ' ++ s
  where len = length s

------------------------------------------------------------------------------
-- Ex 5: let's make a countdown for a rocket! Given a number, you
-- should produce a string that says "Ready!", counts down from the
-- number, and then says "Liftoff!".
--
-- For example,
--   countdown 4 ==> "Ready! 4... 3... 2... 1... Liftoff!"
--
-- Hints:
-- * you can combine strings with the ++ operator
-- * you can use the show function to convert a number into a string
-- * you'll probably need a recursive helper function

-- [한국어 해석] "Ready!" 다음에 n, n-1, ..., 1을 "... " 형태로 이어붙이고 마지막에 "Liftoff!"를 출력합니다. n<=0이면 바로 "Ready! Liftoff!".
countdown :: Integer -> String
countdown n
  | n <= 0    = "Ready! Liftoff!"
  | otherwise = "Ready! " ++ go n ++ "Liftoff!"
  where
    go k
      | k <= 0    = ""
      | otherwise = show k ++ "... " ++ go (k - 1)

------------------------------------------------------------------------------
-- Ex 6: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number evenly.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!
--
-- Hint: remember the mod function!

-- [한국어 해석] 1보다 큰 가장 작은 약수를 찾습니다. 2부터 나누어 보고 나누어떨어지면 그 수를 반환합니다. 어떤 수로도 안 나뉘면 자기 자신(소수)입니다.
smallestDivisor :: Integer -> Integer
smallestDivisor n = findDiv 2
  where
    findDiv k
      | k * k > n      = n
      | n `mod` k == 0 = k
      | otherwise      = findDiv (k + 1)

------------------------------------------------------------------------------
-- Ex 7: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

-- [한국어 해석] 0과 1은 소수가 아니며, 그 외에는 smallestDivisor n == n 인지를 확인해 소수 여부를 판정합니다.
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = smallestDivisor n == n

------------------------------------------------------------------------------
-- Ex 8: implement a function biggestPrimeAtMost that returns the
-- biggest prime number that is less than or equal to the given
-- number. Use the function isPrime you just defined.
--
-- You don't need to care about arguments less than 2. Any behaviour
-- for them is fine.
--
-- Examples:
--   biggestPrimeAtMost 3 ==> 3
--   biggestPrimeAtMost 10 ==> 7

-- [한국어 해석] n 이하에서 가장 큰 소수를 찾습니다. n부터 내려가며 isPrime이 참인 값을 반환합니다. n<2인 경우 0을 반환합니다.
biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost n
  | n < 2     = 0
  | isPrime n = n
  | otherwise = biggestPrimeAtMost (n - 1)
