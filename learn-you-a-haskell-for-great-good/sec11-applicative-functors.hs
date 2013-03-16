import Control.Applicative
-- ↓の定義はControl.Applicativeの内容を写経しているだけなのでimportすれば事足りる

-- Functorの型クラスメソッドはただひとつ "fmap" のみ。
--   Prelude> :type fmap
--   fmap :: Functor f => (a -> b) -> f a -> f b

-- 関数の持ち上げ(lifting): 関数 a -> b を取って 関数 f a -> f b を返す。

-- IOのFunctor instanceがどのように定義されているか？
-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

-- Prelude Data.Char Data.List> intersperse ',' "hogehoge"
-- "h,o,g,e,h,o,g,e"

-- 今まで使って来た "(->) r" もFunctorのinstance.
-- Control.Monad.Instances を覗くと、↓のような実装がある
-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))

-- 以下の二つの視点が同時に成り立つ。
-- fmapとは: 関数とFunctor値を取って、その関数でFunctor値を写して返すもの
-- fmapとは: 値から値への関数を取って、それをFunctor値からFunctor値への関数に持ち上げた(?)ものを返す関数

-- Prelude> fmap (replicate 3) [1,2,3,4]
-- [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]

-- Functor法則 -- Functorを自作するときはFunctor則を満たしているかどうか自前でテストする必要がある。
-- Functor第一法則: idでFunctor値を写した場合、Functor値が変化してはいけない
--                  fmap id = id
-- Prelude> fmap id (Just 3)
-- Just 3

-- Functor第二法則: fとgの合成関数でFunctor値を写したものは、
--                  まずg, 次にfでFunctor値を写したものに等しい
--                  fmap (f . g) = fmap f . fmap g

-- Applicative Functor
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- *Main> import Control.Applicative
-- *Main Control.Applicative> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- *Main> Just (+3) <*> Just 9
-- Just 12

-- MaybeのApplicative instance. 定義は以下の通り
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

-- list []もApplicative Functor. 定義は以下の通り
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- *Main> [(+), (*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]


-- Applicative則
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u


-- Applicativeの便利な関数たち
--   "liftA2"
sequenceA :: (Applicative f) => [f a] -> f a
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs


