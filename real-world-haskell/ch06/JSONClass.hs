{-# LANGUAGE TypeSynonymInstances #-}
module JSONClass
    (
        JAry(..) -- (..)はこの型の詳細全てをexportするという意味
    ) where

type JSONError = String

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue) -- was [(String, JValue)]
            | JArray (Jary JValue)  -- was [JValue]
              deriving (Eq, Ord, Show)

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

-- Haskellはいつでも新しい型クラスのインスタンスを生成できる.
doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id


-- 6.9 重複インスタンスのないJSON型クラス
-- リスト型を包み込んでコンパイラからはリストに見えないようにする.
newtype JAry a = JAry {
    fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
      fromJObj  :: [(String, a)]
    } deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

jaryToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue

listToJValue :: (JSON a) => [a] -> [JValue]
listToJValue = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

-- こうして包み込むだけだとパフォーマンス上のコストは掛からない.

jaryOfValuesToJValue :: JAry JValue -> JValue
jaryOfValuesToJValue = JArray

-- 関数合成で組み立ててしまう. jaryToJValueは上に型シグネチャ(だっけ)書いた関数.
jaryToJValue = JArray . Jary . map toJValue . fromJAry

jaryFromJValue (JArray (JAry a)) =
    whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

-- whenRightはRailsのtryみたいなもんか.
-- Leftのときは触らないけどRightのときはfを作用させる.
whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

-- JObj型に隠れているリストの場合はさらに処理が必要
import Control.Arrow (second)

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

