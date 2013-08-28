-- prittyに出力するライブラリを作る

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

doubl :: Double -> Doc
double d = text (show d)

-- ...
-- あと, <>という関数を用意する. Docをふたつ引数にとりそれを連携する

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

-- Emptyは数学の視点で見れば"連結の単位元"である.
-- (0は加算の単位元, 1は乗算の単位元)

