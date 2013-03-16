import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
-- ↓ ByteStringにしてMonoid実装を活かす
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- *Main> ("beans", Sum 10) `applyLog` addDrink
-- ("milk",Sum {getSum = 35})

-- Writer w a型の実体はタプルのnewtype wrapper.
-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- Monad instanceの定義
-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)
--     (Writer (x, v)) >>= f = let (Writer (y, v')) = f x
--                               in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)


-- プログラムにログを追加しよう
-- 単純なgcdはこう。
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0    = a
    | otherwise = gcd' b (a `mod` b)

-- Writerを追加するとこうなる
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)

-- *Main> mapM_ putStrLn $ snd $ runWriter (gcd'' 8 3)
-- 8 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- Finished with 1

-- リストの末尾に結合していくと非効率。例として逆順の


-- 差分リストを使ってもっと効率的に。
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcd''' :: Int -> Int -> Writer (DiffList String) Int
gcd''' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd''' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

-- *Main> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd''' 110 34
-- Finished with 2
-- 8 mod 2 = 0
-- 34 mod 8 = 2
-- 110 mod 34 = 8

-- 性能を比較する
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])


