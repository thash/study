-- importはかならず頭に書く必要がある
import Data.Char
import Data.List
import qualified Data.Map as Map

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


-- 与えられた数値をshowして, 文字列 "123" になる。
-- 文字列はCharのlistなので、mapでそれぞれにdigitToIntを作用させる。
-- digitToInt '3' => 3 (Int)
-- あとは数値のlistとなったそれに対してsumを適用、数値的に足し合わせる。
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

-- findとMaybe型について
-- *Main> :t find
-- find :: (a -> Bool) -> [a] -> Maybe a
-- *Main> find (>4) [1,2,3]
-- Nothing
-- *Main> find (>4) [1,2,3,4,5,6]
-- Just 5 ... Justとな？

-- *Main> :t Just
-- Just :: a -> Maybe a

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- *Main> firstTo 40
-- Just 49999

-- キーから値のマッピング.
-- 引数はkeyと, 調べるターゲットのlist...そのlistは、各要素が(k,v)のタプルであるようなlist.
findKey_1 :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey_1 key [] = Nothing
findKey_1 key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey_1 key xs


-- もっと再帰を活かして！
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key xs = foldr
                   (\(k,v) acc -> if key == k then Just v else acc)
                   Nothing xs

-- *Main> findKey 2 [(2,4),(3,6)]
-- Just 4

-- 実はfindKeyに相当するlookupというのが, Data.Mapにある。
-- *Main> :t Map.lookup
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "459-1234")
    ,("cany", "12-8302")
    ]

-- *Main> Map.lookup "betty" phoneBook
-- Just "555-2938"
