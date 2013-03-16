-- *Main> :t foldl
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- *Main> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- もっと簡単にかける。
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- "++"関数は:よりもはるかに遅いので、リストから新しいリストを構築する際には普通は右たたみ込み(foldr)を使います。と

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs


-- scanl/rは基本的にfoldl/rと同じだが中間状態を配列で返す。デバッグとかに。
-- *Main> scanl (+) 0 [3,5,2,1]
-- [0,3,8,10,11]

-- 関数適用演算子"$"
--   カッコの数を減らしたり。
-- *Main> :t ($)
-- ($) :: (a -> b) -> a -> b
--
-- *Main> sum $ filter (> 10) $ map (*2) [2..10]
-- 80

-- 関数合成演算子"."
-- *Main> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- *Main> map (negate . abs) [5,-3,-6,7,-3,2]
-- [-5,-3,-6,-7,-3,-2]
-- Rubyのメソッドチェイン的なことができる
-- "ポイントフリースタイル"

fn x = ceiling (negate (tan (cos (max 50 x))))
-- ↑等価↓
fn' = ceiling . negate . tan . cos . max 50


oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (< 10000) . filter odd $ map (^2) [1..]

-- filter odd が map(^2) [1..]に適用され、それからtakeWhile (<10000) がその結果に適用され、その結果にsumが適用される


