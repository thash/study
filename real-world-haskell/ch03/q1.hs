-- q1: 本体
-- q2: 型シグネチャの追加
countItems :: [a] -> Int
countItems     [] = 0
countItems (x:xs) = 1 + countItems xs

-- *Main> :t countItems
-- countItems :: [a] -> Int
