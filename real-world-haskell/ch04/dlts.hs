-- 4.10
import Data.List (isPrefixOf)

-- C言語のヘッダファイルからマクロ名を抽出する.
dlts :: String -> [String]
dlts = foldr step [] . lines
    where step l ds | "#define DLT_" `isPrefixOf` l = secondWord l : ds
                    | otherwise = ds
          secondWord = head . tail . words

-- foldr使うより, map + filterの定義がわかりやすいんじゃないか, という話.
-- ついでにstepとsecondWordの内容も展開してるよ.
-- dlts' = map (head . tail . words) . filter ("#define DLT" `isPrefixOf`) . lines

-- foldは1個1個みていくからwordsとか苦手
-- 全部はfoldで
-- 再帰(loop, を使った明示的再帰)を書いてみたらfoldになった, というのはよくある.
