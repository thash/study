main = do
    -- さっそく束縛
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- Haskellのreturnは実行を終了しない．returnは結果を生成するIOアクションを作り出し，結果を捨てる．
-- 捨てたくなければ a <- return "hi" とする
