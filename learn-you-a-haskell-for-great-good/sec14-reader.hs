import Control.Monad.Instances

-- 関数の型 (->) r はFunctorであり、Applicative Functorであるばかりでなく、Monadでもある。
-- Control.Monad.Instances に以下のような定義がある
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w

-- ここでのreturnはpureとほぼ同じ

-- 関数モナドを使ったdo式は次のように書ける
addStuff :: Int -> int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)

-- すべての関数が共通の情報を「読む」ため, 関数は"Readerモナド"とも呼ばれる。
-- 関数が返すであろう値を既に知っているつもりが出来る。

