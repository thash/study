import Control.Monad

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1)
               ,(c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

-- guardで, 8x8マスに入っているものだけを選別

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

-- もしくはdoを使わずbindを明示的に使って次のようにも書ける。
-- in3 :: KnightPos -> [KnightPos]
-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- *Main> (6,2) `canReachIn3` (6,1)
-- True

