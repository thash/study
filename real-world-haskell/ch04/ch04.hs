-- foldlは遅延評価(サンクthunkを作る), foldl'は正格評価.
-- オーバーヘッドとなる見えないサンク作成を"スペースリーク"と呼ぶ. 本文では
-- > "foldlはサンクを作るので, たとえ失敗する可能性がなくとも不必要に非効率ですから,
-- >  実際のプログラムでは使わないというのが賢い選択です"
-- と描いてるけど遅延したいときに使ってもよくね, とか話してた.


-- 4.7 無名(ラムダ)関数
----------------------------------------------------------------------

isInAny needle haystack = any inSequence haystack
   where inSequence s = needle `isInfixOf` s

-- 無名関数で同じ事をやると...
isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack


-- 無名関数で出来ないこと:
safeHaed (x:_) = Just x
safeHaed _     = Nothing
-- というような'複数のパターンマッチ'ができない.


-- 4.8 部分関数適用とカリー化
----------------------------------------------------------------------

-- Haskellでは"関数の返り値"と"引数"の間が同じ -> で分離されている.
-- その真実が明かされる.

Prelude> let zip3foobar = zip3 "foo" "bar"
Prelude> :type zip3foobar
zip3foobar :: [c] -> [(Char, Char, c)]
Prelude> zip3foobar "quux"
[('f','b','q'),('o','a','u'),('o','r','u')]

-- 上述のisInAnyを部分関数適用を使って書くと...
isInAny3 needle haystack = any (isInfixOf needle) haystack
-- ここで, isInfixOfは1個しか引数を与えられず, ()内は適用済み関数となる.
-- haystackのうちanyがneedle, ならTrueという関数が出来上がる.

-- 部分関数適用 != カリー化
-- カリー化された関数のみが, 部分関数適用することができる.

-- 雑談:
-- > Groovyでは、部分適用を行う標準メソッドにcurryという名前がつけられている
-- > コンビネータ論理(Combinatory Logic、組み合わせ論理)は、Moses Schönfinkelとハスケル・カリーによって、記号論理での変数を消去するために導入された記法である
-- > プログラムとはクライスリ圏の射である（program is arrow of Kleisli category）


-- 4.9 アズパターン(As-pattern)
----------------------------------------------------------------------

xs@(_:xs') -- これをアズパターンと呼ぶ.
-- 変数xsを, @記号の右辺とマッチした値"全体"に束縛する という意味.

sufixes :: [a] -> [[a]]
sufixes xs@(_:xs') = xs : sufixes xs'
sufixes _          = []

Prelude> import Data.List
Prelude Data.List> tails "foo"
["foo","oo","o",""]
Prelude> sufixes "foo"
["foo","oo","o"]

-- もしアズパターンを使わないと...
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _      = []
-- 2行目で, せっかくマッチして分解したのに結局:でconsして全リスト作り直している.

xs@(_:ys@(_:ys'))
-- みたいに入れ子適用もできる.

-- さらにこれはリストにも限らなくて,
-- x@(...) は (...) をxに束縛する, という一般的な表現に適用可能.

