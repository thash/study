-- "強い"型システムとは, 「プログラムにある種のエラーがないことを保証する」もの.
-- Haskellには"実行時の型エラー"が存在しない(コンパイラが書く時にすべて解決することを要求).
-- PerlやJavaScriptでは"foo" + 2が実行可能だが, Haskellではエラーとなる
--     <interactive>:26:7:
--         No instance for (Num [Char]) arising from a use of `+'
--         Possible fix: add an instance declaration for (Num [Char])
--         In the expression: "foo" + 2
--         In an equation for `it': it = "foo" + 2

-- ghci> :load file.hs ... ファイルをロード


-- Haskellの強い型システムでは, 強制型変換は行われない.

Prelude> 1
1
it :: Integer
Prelude> 3 :: Int -- 明示的に型を指定.
3
it :: Int

-- Haskellはデフォの型決定基準, 優先度を持ってる.

-- X86+SSE2, のときはFloatのほうがいいとか?
-- > http://book.realworldhaskell.org/read/types-and-functions.html
-- SSE: ストリーミングSIMD拡張命令 (Streaming SIMD Extensions, SSE) は、インテルが開発したCPUのSIMD拡張命令セット

-- Prelude> compare 6 5
-- GT
-- Prelude> :type it
-- it :: Ordering
-- 順序を表す Ordering 型.

-- 関数適用はどんな演算子よりも優先度が高い
-- Prelude> compare 2 3 == LT
-- True

Prelude> :type [(True, []), (False, [['a']])]
[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

-- 空リストを持つリストは空リストそのものとは別物.
Prelude> [[]] /= []
True


Prelude> fst (1,'a')
1
Prelude> fst (1,'a',"lslsls")
<interactive>:5:5:
    Couldn't match expected type `(a0, b0)'
                with actual type `(t0, Char, [Char])'
    In the first argument of `fst', namely `(1, 'a', "lslsls")'
    In the expression: fst (1, 'a', "lslsls")
    In an equation for `it': it = fst (1, 'a', "lslsls")
Prelude> :type fst
fst :: (a, b) -> a
Prelude> :type snd
snd :: (a, b) -> b


-- let XXX in YYY
-- XXXで定義したものはYYY中でしか使えない

-- 関数適用は左結合
-- a b c d は (((a b) c) d) と同じ

-- 関数に:typeを作用させた時の->は "to" と読む.

-- 副作用がある関数は型シグネチャにIO Stringが出てくる.
-- ghciと.hsファイルでの微妙な書き方の違いの根源はIOモナドにある. 7章を待て

Prelude> :type getLine
getLine :: IO String

Prelude> :load Assign.hs
--[1 of 1] Compiling Main             ( Assign.hs, interpreted )
-- Assign.hs:2:1:
--     Multiple declarations of `x'
--     Declared at: Assign.hs:1:1
--                  Assign.hs:2:1
-- Failed, modules loaded: none.


-- in ghci
Prelude> x = 102
<interactive>:3:3: parse error on input `='

Prelude> let x = 102
Prelude> x
102

-- ghciでは x = 1, とするとエラー. let x = 1, のようにletを付ける必要がある.
-- ファイルの代入だとエラーなんだけどghciのletだと再束縛ok.

