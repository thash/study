module SimpleJSON
    ( -- このカッコはexport定義.
    JValue(..) -- なんだこれ
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where -- 以降がmodule本体だよ的なつなぎのwhere.

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

-- *Main> JString "Foo"
-- JString "Foo"
-- *Main> :type JBool
-- JBool :: Bool -> JValue
-- *Main> :type JBool True
-- JBool True :: JValue

getInt (JNumber n) = Just (truncate n) -- 整数に丸める
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _         = Nothing

getArray (JArray b) = Just b
getArray _         = Nothing

isNull v = v == JNull

-- 1ツのHaskellソースファイルは1ツ以下のモジュール定義を含む.
-- さらにいちばん上に書かないといけない(というわけでここで一番上に追記)

