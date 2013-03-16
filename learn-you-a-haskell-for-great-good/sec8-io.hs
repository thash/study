-- 人間からコンピュータに与えるのは、一連の実行ステップではなく、あるものが何であるかの定義です。

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- *Main> :t getLine
-- getLine :: IO String
-- putStrLnもgetLineも、IOアクションである。それをdoを使って糊付けすればひとつのI/Oアクションになる。


import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey" ++ bigFirstName ++ " "
                     ++ bigLastName
                     ++ ", how are you?"



