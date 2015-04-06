-- 人間からコンピュータに与えるのは、一連の実行ステップではなく、あるものが何であるかの定義です。

-- () は unit. putStrLnは意味のある返り値を持たないのでダミーとして．
-- Prelude> :t putStrLn
-- putStrLn :: String -> IO ()

-- doのあとにずらずらIOアクションを並べると，それがひとつのIOアクションに"のりづけ"される
-- doを書いた後はあたかも命令型プログラムのように実行ステップを書き並べる
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine -- Stringを生成するIOアクション
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- *Main> :t getLine
-- getLine :: IO String
-- putStrLnもgetLineも、IOアクションである。それをdoを使って糊付けすればひとつのI/Oアクションになる。

-- "hoge" ++ getLine とか， var = getLine
-- みたいな書き方はできない．どこかで<-を使って束縛するしかない

import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    -- letは<-と違って「普通の(純粋な)値にに名前を与える」
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey" ++ bigFirstName ++ " "
                     ++ bigLastName
                     ++ ", how are you?"



