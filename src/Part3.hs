module Part3 where
import Data.List (group, delete, sort, nub)

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 2 = True
prob18 n = getDividers n == [n]

getDividers :: Integer -> [Integer]
getDividers  = currentDividers 2
  where
    currentDividers :: Integer -> Integer -> [Integer]
    currentDividers _ 1 = []
    currentDividers divisor n
      |divisor * divisor > n = [n]
      |n `mod` divisor == 0 = divisor : currentDividers divisor (n `div` divisor)
      |otherwise = currentDividers (succ divisor) n

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 n = map (\divs -> (head divs, length divs)) (group (getPrimeDividers n))

getPrimeDividers :: Integer -> [Integer]
getPrimeDividers  = primeDividers 2
  where
    primeDividers :: Integer -> Integer -> [Integer]
    primeDividers _ 1 = []
    primeDividers divider n | divider * divider > n = [n]
                            | n `mod` divider == 0 = divider : primeDividers divider (n `div` divider)
                            | otherwise = primeDividers (succ divider) n

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = n == sum (divisors n)

divisors :: Integral a => a -> [a]
divisors n = (l++) $ nub
  $ concat [[x, n `div` x] | x <- [2..floor . sqrt $ fromIntegral n],
  n `mod` x == 0]
    where
      l = if n == 1 then [] else [1]

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = (sort . divisors) n ++ [n]

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product $ (map count) (words str)
    where
      count :: String -> Integer
      count wght = toInteger $ length (filter(=='i') wght)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 num = checkNum(sqrt (1 + 8 * fromInteger num)) == 0
  where
    checkNum x = x - fromIntegral (floor x)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 x = reverser x == x

reverser :: Integral a => a -> a
reverser = funct 0
  where
    funct a 0 = a
    funct a b = let (q, r) = quotRem b 10 in funct (a * 10 + r) q

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 m n = sum (dividers m) == n && sum (dividers n) == m

dividers :: Integer -> [Integer]
dividers k = [x | x <- [1..k - 1], k `mod` x == 0]
------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 n list = first [(x, y)| x <- list, y <- delete x list, x + y == n]

first :: [a] -> Maybe a
first []     = Nothing
first (x:xs) = Just x
------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 n list = first [(v, x, y, z) | x <- list, y <- delete x list, z <- delete y (delete x list), v <- delete z (delete y (delete x list)), x + y + z + v == n]

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = maximum [x * y|
    x <- [minNumber..maxNumber], y <-[minNumber..maxNumber],
    (prob25 . toInteger) (x*y)]
      where
        minNumber = 10 ^ (k - 1)
        maxNumber = 10 ^ k - 1

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 k = head (filter (\t -> length ([x | x <- [1..t], t `mod` x == 0]) >= k) (map (\n -> n * (n + 1) `div` 2) [0..]))

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 n = sum [x + y |x <- [1 .. n],y <- [x+1 .. n], prob26 (toInteger x) (toInteger y)]

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 list n = error "Implement me!"
