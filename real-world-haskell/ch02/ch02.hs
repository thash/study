-- "強い"型システムとは, 「プログラムにある種のエラーがないことを保証する」もの.
-- Haskellには"実行時の型エラー"が存在しない(コンパイラが書く時にすべて解決することを要求).
-- PerlやJavaScriptでは"foo" + 2が実行可能だが, Haskellではエラーとなる
--     <interactive>:26:7:
--         No instance for (Num [Char]) arising from a use of `+'
--         Possible fix: add an instance declaration for (Num [Char])
--         In the expression: "foo" + 2
--         In an equation for `it': it = "foo" + 2

-- ghci> :load file.hs ... ファイルをロード
