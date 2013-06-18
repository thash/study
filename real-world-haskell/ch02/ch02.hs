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

-- Prelude> :type [(True, []), (False, [['a']])]
-- [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

-- 空リストを持つリストは空リストそのものとは別物.
-- Prelude> [[]] /= []
-- True


