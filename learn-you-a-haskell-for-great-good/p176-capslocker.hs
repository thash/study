import Control.Monad
import Data.Char

-- main = forever $ do
--     l <- getLine
--     putStrLn $ map toUpper l

-- when execute this code,  at last we met this messagee.
--   p176-capslocker: <stdin>: hGetLine: end of file
-- here, use getContents!

main = do
    contents <- getContents
    putStr $ map toUpper contents

