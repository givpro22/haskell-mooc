-- Welcome to the first exercise set of the Haskell Mooc! Edit this
-- file according to the instructions, and check your answers with
-- 첫 번째 연습 세트에 오신 것을 환영합니다! 지침에 따라 이 파일을 편집하고 답변을 확인하세요.
--
--   stack runhaskell Set1Test.hs
--
-- You can also play around with your answers in GHCi with
-- GHCi에서 답변을 테스트할 수도 있습니다.
--
--   stack ghci Set1.hs
--
-- This set contains exercises on
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion
--
-- 이 세트에는 다음 주제에 대한 연습 문제가 포함되어 있습니다.
--   * 함수 정의
--   * 기본 표현식
--   * 패턴 매칭
--   * 재귀

module Set1_copy where

import Mooc.Todo

-- stack ghci Set1_copy.hs
-- stack runhaskell Set1Test_copy.hs

------------------------------------------------------------------------------
-- Ex 1: define variables one and two. They should have type Int and
-- values 1 and 2, respectively.
-- Ex 1: 변수 one과 two를 정의하세요. 이들은 각각 Int 타입이고 값은 1과 2여야 합니다.

one :: Int
one = 1
two :: Int
two = 2

------------------------------------------------------------------------------
-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.
-- Ex 2: Integer->Integer 타입의 함수 double을 정의하세요. double은 하나의 인자를 받아 두 배로 곱한 값을 반환해야 합니다.

double :: Integer -> Integer
double x = x * 2

------------------------------------------------------------------------------
-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.
-- Ex 3: 이전 연습 문제의 함수 double을 사용하여 인자를 네 배로 곱한 값을 반환하는 함수 quadruple을 정의하세요.

quadruple :: Integer -> Integer
quadruple x = double (double x)

------------------------------------------------------------------------------
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance
-- between points (x1,y1) and (x2,y2).
--
-- Give distance a type signature, i.e. distance :: something.
--
-- PS. if you can't remember how the distance is computed, the formula is:
--   square root of ((x distance) squared + (y distance) squared)
--
-- Examples:
--   distance 0 0 1 1  ==>  1.4142135...
--   distance 1 1 4 5  ==>  5.0



-- Ex 4: 함수 distance를 정의하세요. 이 함수는 Double 타입의 네 인자 x1, y1, x2, y2를 받아서 점 (x1,y1)과 (x2,y2) 사이의 (유클리드) 거리를 반환해야 합니다.
--
-- distance에 타입 시그니처를 명시하세요, 예: distance :: something.
--
-- 참고: 거리를 계산하는 방법이 기억나지 않는다면, 공식은 다음과 같습니다:
--   ((x 거리)^2 + (y 거리)^2)의 제곱근
--
-- 예시:
--   distance 0 0 1 1  ==>  1.4142135...
--   distance 1 1 4 5  ==>  5.0

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2= (sqrt((x2 - x1)^2 + (y2 - y1)^2))

------------------------------------------------------------------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"



-- Ex 5: 짝수 입력에 대해 "eeny"를, 홀수 입력에 대해 "meeny"를 반환하는 함수 eeny를 정의하세요.
--
-- 참고: 내장 함수 "even"을 참고하세요.

eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"

------------------------------------------------------------------------------
-- Ex 6: here's the function checkPassword from the course material.
-- Modify it so that it accepts two passwords, "swordfish" and
-- "mellon".


 
-- Ex 6: 강의 자료에 있는 함수 checkPassword입니다.
-- 이 함수가 "swordfish"와 "mellon" 두 개의 비밀번호를 허용하도록 수정하세요.

checkPassword :: String -> String
checkPassword password = if password == "swordfish"
                         then "You're in."
                         else if password == "mellon"
                              then "You're in."
                              else "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7: A postal service prices packages the following way.
-- Packages that weigh up to 500 grams cost 250 credits.
-- Packages over 500 grams cost 300 credit + 1 credit per gram.
-- Packages over 5000 grams cost a constant 6000 credits.
--
-- Write a function postagePrice that takes the weight of a package
-- in grams, and returns the cost in credits.

-- Ex 7: 우편 서비스는 다음과 같이 소포 가격을 책정합니다.
-- 500그램 이하 소포는 250 크레딧입니다.
-- 500그램 초과 소포는 300 크레딧에 1그램당 1 크레딧이 추가됩니다.
-- 5000그램 초과 소포는 6000 크레딧으로 고정됩니다.
--
-- 소포 무게(그램 단위)를 받아 비용(크레딧 단위)을 반환하는 함수 postagePrice를 작성하세요.

postagePrice :: Int -> Int
postagePrice x = if x <= 500 then 250
                 else if x <= 5000 then 300 + x
                      else 6000

------------------------------------------------------------------------------
-- Ex 8: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. remember, the type of booleans in haskell is Bool

-- Ex 8: 정수가 0이면 True를, 그렇지 않으면 False를 반환하는 함수 isZero를 정의하세요. isZero에 타입 시그니처를 명시하세요.
--
-- 패턴 매칭을 사용하세요! 비교 연산은 사용하지 마세요!
--
-- 참고: Haskell에서 불리언 타입은 Bool입니다.

isZero :: Integer -> Bool

isZero 0 = True
isZero _ = False 

------------------------------------------------------------------------------
-- Ex 9: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n
-- Ex 9: 재귀를 사용하여 sumTo 함수를 구현하세요.
-- sumTo n은 1+2+...+n의 합을 계산합니다.

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)
  
--sumTo n = if n==0 then 0
--          else sumTo(n-1) + n

------------------------------------------------------------------------------
-- Ex 10: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

-- Ex 10: power n k는 n의 k제곱 (즉, n^k)을 계산해야 합니다.
-- 재귀를 사용하세요.

power :: Integer -> Integer -> Integer
power n k = if k==0 then 1
            else n * power n (k - 1)

------------------------------------------------------------------------------
-- Ex 11: ilog3 n should be the number of times you can divide given
-- number by three (rounding down) before you get 0.
--
-- For example, ilog3 20 ==> 3 since
--   20/3 = 6.66 (gets rounded down to 6)
--   6/3 = 2
--   2/3 = 0.666 (gets rounded down to 0)
--
-- Use recursion to define ilog3. Use the function "div" for integer
-- division. It rounds down for you.
--
-- More examples:
--   ilog3 2 ==> 1
--   ilog3 7 ==> 2


-- Ex 11: ilog3 n은 주어진 수를 3으로 나누었을 때(내림하여) 0이 되기 전까지 나눌 수 있는 횟수입니다.
--
-- 예를 들어, ilog3 20 ==> 3입니다. 이유는:
--   20/3 = 6.66 (내림하여 6)
--   6/3 = 2
--   2/3 = 0.666 (내림하여 0)
--
-- 재귀를 사용하여 ilog3를 정의하세요. 정수 나눗셈에는 "div" 함수를 사용하세요. 이 함수는 내림을 수행합니다.
--
-- 추가 예시:
--   ilog3 2 ==> 1
--   ilog3 7 ==> 2

ilog3 :: Integer -> Integer
ilog3 x= if x == 0 
         then 0 
         else ilog3(x `div` 3) + 1