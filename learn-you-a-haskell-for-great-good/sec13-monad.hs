-- Monadとは, >>= をサポートするApplicative Functorに過ぎない。

-- Maybeに対して >>=を定義してみる。applyMaybeととりあえず呼ぼうか。
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- *Main> Just 3 `applyMaybe` \x -> Just (x + 1)
-- Just 4
-- *Main> Nothing `applyMaybe` \x -> Just (x ++ " :)")
-- Nothing

-- Monadの型クラス, Monad!
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg


-- MaybeのMonad instance 実装は次のようになる。
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

-- *Main> return "WHAT" :: Maybe String
-- Just "WHAT"
-- *Main> Just 9 >>= \x -> return (x*10)
-- Just 90
-- *Main> Nothing >>= \x -> return (x*10)
-- Nothing


-- HaskellにとってMonadはとても便利なので、モナド専用構文まで用意されています。その名はdo記法。実は、do記法はIOモナドだけじゃなくあらゆるモナドに使えます。

-- Monad則
-- 1. 左恒等性
return x >>= f = f x
-- 2. 右恒等性
m >>= return = m
-- 3. 結合法則
(m >>= f) >>= g = m >>= (\x -> f x >>= g)


