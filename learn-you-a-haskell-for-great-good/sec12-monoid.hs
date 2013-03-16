-- Monoidとは
--   * 二項演算子
--   * 単位元 (+に対しては0, *に対しては1, ++に対しては[])
-- の2要素からなる構造である。定義は次のようになっている。

import Data.Monoid-- 定義は Data.Monoid の中にある。
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

-- Monoid則
mempty `mappend` x = x -- 単位元が単位元であること
x `mappend` mempty = x -- 同じく
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) -- 交換則

-- OrderingもMonoid instanceにできる。
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

-- Maybe Monoid
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

