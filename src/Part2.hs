module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 colorLetter = case colorLetter of
  RED -> 'R'
  GREEN -> 'G'
  BLUE -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 p = (getDataValue p) <= 255 && (getDataValue p) >= 0

getDataValue :: ColorPart -> Int
getDataValue (Red int) = int
getDataValue (Green int) = int
getDataValue (Blue int) = int

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 c cp = case cp of
    Red cv -> c {red = red c + cv}
    Green cv -> c {green = green c + cv}
    Blue cv -> c {blue = blue c + cv}

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 cp = getDataValue cp

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 (Color r g b) | (r > g) && (r > b) = Just (Red r)
                     | (g > r) && (g > b) = Just (Green g)
                     | (b > r) && (b > g) = Just (Blue b)
                     | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = root tree + sumof (left tree) + sumof (right tree)
  where
    sumof Nothing = 0
    sumof (Just x) = prob11 x

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 tree = checkRight (right tree) (root tree) && checkLeft (left tree) (root tree)

checkRight :: Ord a => Maybe (Tree a) -> a -> Bool
checkRight Nothing x = True
checkRight (Just tree) parent = root tree >= parent && checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)

checkLeft :: Ord a => Maybe (Tree a) -> a -> Bool
checkLeft Nothing x = True
checkLeft (Just tree) parent = root tree < parent && checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)


------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 n tree = subRec n (Just tree)
  where
    subRec :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
    subRec _ Nothing = Nothing
    subRec n (Just (Tree l root r))
      | n == root = (Just (Tree l root r))
      | n < root = subRec n l
      | n > root = subRec n r

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 t = case enumerate (Just t) 1 of
    (Just enumerated, _) -> enumerated

enumerate :: Maybe (Tree ()) -> Int -> (Maybe (Tree Int), Int)
enumerate Nothing i = (Nothing, i)
enumerate (Just (Tree l () r)) i = (Just $ Tree l' current r', current + 1)
    where
        (r', afterRight) = enumerate r i
        (l', afterLeft) = enumerate l afterRight
        current = afterLeft

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree lr (right tree)
  where
    lr subTree = subTree {left = Just oldTree}
      where
        oldTree = tree {right = left subTree}

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rr (left tree)
  where
    rr subTree = subTree {right = Just oldTree}
      where
        oldTree = tree {left = right subTree}

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
