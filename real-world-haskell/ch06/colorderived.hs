-- colorderived.hs
data Color = Red | Green | Blue
    deriving (Read, Show, Eq, Ord)
-- Haskell標準は, 上の特定の型クラス(この4ツ?)については
-- コンパイラは自動的に導出できなければならないとしています.

-- *Main> (read "Red")
-- <interactive>:8:2:
--     No instance for (Read a0) arising from a use of `read'
--     The type variable `a0' is ambiguous
--     Possible fix: add a type signature that fixes these type variable(s)
--     Note: there are several potential instances:
--       instance Read Color
--         -- Defined at real-world-haskell/ch06/colorderived.hs:2:15
--       instance Read () -- Defined in `GHC.Read'
--       instance (Read a, Read b) => Read (a, b) -- Defined in `GHC.Read'
--       ...plus 26 others
--     In the expression: (read "Red")
--     In an equation for `it': it = (read "Red")

-- *Main> (read "Red")::Color
-- Red

-- *Main> (read "[Red,Red,Blue]")::[Color]
-- [Red,Red,Blue]


-- AutomaticDerivation.hs
data CannotShow = CannotShow
data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)

-- SimpleResult.hs

-- JSONClass.hs
instance JSON Bool where -- "Bool型をJSONクラスのインスタンスにしてる"??
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
-- right(右, 正しい) なのでRightが正しくLeftがエラーというケースが多い


