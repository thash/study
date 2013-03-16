import qualified Data.Map as Map

-- data Shape = Circle Float Float Float |
--               Rectangle Float Float Float Float
--     deriving (Show)

-- Circle, Rectangleは値コンストラクタと呼ばれる。その実、Shapeを返すただの関数である。
-- *Main> :t Circle
-- Circle :: Float -> Float -> Float -> Shape

-- area :: Shape -> Float
-- area (Circle _ _ r) = pi * r ^ 2
-- area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 値コンストラクタはpattern matchにも使える
-- *Main> area $ Circle 10 20 10
-- 314.15927


-- データを表示するためにderiving (Show)する。
-- 二次元空間の点を表すPointを用意、そいつを使ってShapeを定義する。

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- この形式のShapeを使った関数を作る。パターンのみ変更。
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- *Main> area (Rectangle (Point 0 0) (Point 10 10))
-- 100.0
-- *Main> area (Circle (Point 1 2) 3)
-- 28.274334

-- 図形を移動させる関数。座標モデルを採用した甲斐があるというもの
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
    = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- 点を直接触らせないために

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)


-- レコード構文を使って定義する。
data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     , height    :: Float
                     , phoneNumber :: String
                     , flavor    :: String} deriving (Show)

-- 対応するgetter的な関数も自動的に作ってくれる
-- *Main> :t flavor
-- flavor :: Person -> String

-- その他メモとか
-- 型コンストラクタ。というものがある。↑で出てきたのは"値"コンストラクタ。
-- readはshowの反対で、文字列をreadしてある型のインスタンスを作る.

 -- 型シノニムはtype Hoge = Stringなどとして、意味を読み取りやすい別名を付ける
-- ロッカーのモデル。
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber
                      ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber
                                                  ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZZZZZ"))
    ,(101, (Free, "JJJJJ"))
    ,(103, (Free, "IIIII"))
    ,(105, (Free, "QQQQQ"))
    ,(109, (Taken, "88888"))
    ,(110, (Taken, "99999"))
    ]

-- *Main> lockerLookup 101 lockers
-- Right "JJJJJ"
-- *Main> lockerLookup 109 lockers
-- Left "Locker 109 is already taken!"
-- *Main> lockerLookup 222 lockers
-- Left "Locker 222 doesn't exist!"

-- 7.7. 再帰的なデータ構造
--   進研ゼ...SICPでやったところだ！
--   木構造を作って行こう。

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right


-- *Main> let nums = [8,6,4,1,7,3,5]
-- *Main> let numsTree = foldr treeInsert EmptyTree nums
-- *Main> numsTree
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

-- *Main> 8 `treeElem` numsTree
-- True
-- *Main> 80 `treeElem` numsTree
-- False
-- *Main> 80 `treeInsert` numsTree
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree (Node 80 EmptyTree EmptyTree)))



-- :info で「型クラスのインスタンス」を見ることが出来る
-- Prelude> :info String
-- type String = [Char]    -- Defined in `GHC.Base'


-- Yes/Noクラスを作る。弱い型付けのJavaScript的な動きをemulate.
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

-- カスタム型クラスも。
-- instance YesNo TrafficLight where
--     yesno Red = False
--     yesno _   = True

-- 最初に使うときだけなんかロードしてるよね
-- *Main> yesno $ length []
-- Loading package array-0.4.0.0 ... linking ... done.
-- Loading package deepseq-1.3.0.0 ... linking ... done.
-- Loading package containers-0.4.2.1 ... linking ... done.
-- False
-- *Main> yesno $ length []
-- False

-- Functor.
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (GHC.Base.<$) :: a -> f b -> f a
--         -- Defined in `GHC.Base' class Functor f where

-- GHCiでは:infoのほかに:kindというものも。
--    :info [<name> ...]          display information about the given names
--    :type <expr>                show the type of <expr>
--    :kind <type>                show the kind of <type>

