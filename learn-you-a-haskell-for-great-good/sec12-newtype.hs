-- 「1つの型を取り、それをナニカにくるんで別の型にみせかける」という用途はよくある。
-- dataキーワードでもできるが、より高速なnewtypeキーワードがある。
data ZipList a = ZipList { getZipList :: [a] }
-- これと等価なnewtypeの使い方は...
newtype ZipList a = ZipList { getZipList :: [a] }
-- てか見た目は同じだな

-- 使い方の実例
-- 「MaybeをFunctorのインスタンスにする」ならふつーにできる。
instance Functor Maybe where
    fmap ...
-- とやればいい。一方、
-- 「タプルをFunctorのInstanceにしたい」ときはnewtypeが役立つ。
newtype Pair b a = Pair { getPair :: (a, b) }

