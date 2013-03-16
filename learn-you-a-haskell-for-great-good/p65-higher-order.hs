-- http://ja.wikipedia.org/wiki/高階関数
-- Haskellの関数はCurry化されているため、
-- 足りない引数で呼び出すと部分適用された関数が返る。

-- 最終的な関数の返り値と引数がすべてフラットに書かれている。
-- 考え方としてHaskellの関数は引数は常にひとつで、次の->変換を行う関数を次々に返している。

mulTree :: Int -> Int -> Int -> Int
mulTree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- "a" is a function, b and c are arrays.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- *Main> zipWith' (*) (replicate 5 2) [1..]
-- [2,4,6,8,10]
-- *Main> replicate 5 2
-- [2,2,2,2,2]
-- *Main> take 10 [1..]
-- [1,2,3,4,5,6,7,8,9,10]

-- *Main> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x


-- 関数とリストを受け取り、また別のリストを返す
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- *Main> map' (map (^2)) [[1,2], [3,4,5,6],[7,8]]
-- [[1,4],[9,16,25,36],[49,64]]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

-- *Main> let notNull x = not (null x) in filter' notNull [[1,2,3],[,[3,4,5],[2,2],[,[,[]
-- [[1,2,3],[3,4,5],[2,2]]]]]]

-- map/fiterはリスト内包表記でも書ける。
-- 4章でやったquicksortをfilter'で書いてみる。
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter' (<= x) xs
        larger         = filter' (>  x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger


largestDivisible :: Integer
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- *Main> sum (takeWhile (<10000) (filter' odd (map (^2) [1..])))
-- 166650

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter' isLong (map' chain [1..100]))
    where isLong xs = length xs > 15

-- *Main> numLongChains
-- 66

------ lambda ------

-- iTerm2でバックスラッシュが円マークになるの嫌なんだが...
numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15)
                                (map chain [1..100]))

filp'' :: (a -> b -> c) -> b -> a -> c
filp'' f = \x y -> f y x

