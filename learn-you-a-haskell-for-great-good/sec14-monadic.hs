import Control.Monad

-- Monadを"平らに"してくれるjoin関数. 実装は次の通り

join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m

-- *Main> join (Just (Just 9))
-- Just 9
-- *Main> join [[1,2,3], [3,4,5]]
-- [1,2,3,3,4,5]


-- foldlのモナド版, foldM
-- *Main> foldl (\acc x -> acc + x) 0 [2,8,3,1]
-- 14

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)

-- *Main> foldM binSmalls 0 [2,8,3,1]
-- Just 14
-- *Main>  foldM binSmalls 0 [2,11,3,1]
-- Nothing


-- RPN電卓をMonadicにしてみよう
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

-- *Main> readMaybe "1" :: Maybe Int
-- Just 1
-- *Main> readMaybe "Go to the hell" :: Maybe Int
-- Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

-- *Main> foldingFunction [3,2] "*"
-- Just [6.0]
-- *Main> foldingFunction [] "hoge"
-- Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

-- *Main> solveRPN "1 2 * 4 +"
-- Just 6.0

