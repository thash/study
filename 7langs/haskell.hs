--------------------------------------------------
-- 8.1. Haskellとは
--------------------------------------------------

-- HakellはScala, Erlang, Clojureと違って命令形を許さない徹底的な関数型.
-- I/O処理や状態を蓄積する処理は逆に難しくなる.

-- 1980年代に発展した遅延評価と純粋関数型プログラミングの概念を純化したものがHaskell
-- 強力な型システムによってポリモーフィズムと明快な設計が可能となる.
-- 他にも
--   * パターンマッチングガード
--   * 遅延評価
--   * リスト内包表記
-- などの機能を持つ

-- > Haskellは純粋関数型言語なので副作用がない. その代わり, 関数は副作用を返すことが出来る. この副作用は後で実行できる.
-- > こうした関数の例, およびモナドと呼ばれる概念を利用して状態を保存する
-- p.232

-- $ ghci

-- ローカルスコープで変数に関数をバインドする(ghci内では使うけどファイルload形式だとletいらない)
-- let double x = x * 2

-- フィボナッチ数を求める

module Main where
    fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
    fibTuple (x, y, 0) = (x, y, 0)
    fibTuple (x, y, index) = fibTuple (y, x + y, index - 1)

    fibResult :: (Integer, Integer, Integer) -> Integer
    fibResult (x, y, z) = x

    fib :: Integer -> Integer
    fib x = fibResult (fibTuple (0, 1, x))

-- ghciでテスト.
-- :load <file>.hs
-- [1 of 1] Compiling Main             ( haskell.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> fib 100
-- 354224848179261915075
-- *Main> fib 10000
-- 33644764876431783266621612005107543310....

-- 関数合成
---------------------------------------------------------

-- f(g(x)) をHaskellでは f . g x と表す.
-- 例: tailを適用した結果にheadを適用.

    second = head . tail

-- リストの分解/構築は : 演算子で行う
-- *Main> 1:[2,3]
-- [1,2,3]
-- *Main> let h:t = [9,8,7]
-- *Main> h
-- 9
-- *Main> t
-- [8,7]

-- リスト内包表記
-- *Main> [if even x then x * 2 else 0 | x <- [1..10]]
-- [0,4,0,8,0,12,0,16,0,20]

-- 組み合わせを見つけ出す
-- *Main> let nucs = ["A", "T", "G", "C"]
-- *Main> [(a,b) | a <- nucs, b <- nucs, a /= b]
-- [("A","T"),("A","G"),("A","C"),("T","A"),("T","G"),("T","C"),("G","A"),("G","T"),("G","C"),("C","A"),("C","T"),("C","G")]

-- 設計者の一人にインタビュー
--   * derivedキーワードによってジェネリックなプログラミングをシンプルに実現
--   * 一から作り直せるとしたら分散プログラミングに注力する
--     * 分散プログラミングの世界では, デフォルトで先行評価, 必要なとき遅延評価を使う.
--   * タンパク質の畳み込み解析にもHaskell ;; まじか
--     * http://www.haskellforall.com/2013/10/an-all-atom-protein-search-engine.html こんなんある


-- 無名関数
---------------------------------------------------------
-- (\x -> x) "Logical." -- "Logical."
-- (\(x, y) -> x + y) (1, 3) -- 4

    squareAll list = map square list
                     where square x = x * x

-- 無名関数を使ってこうも書ける

    squareAll2 list = map (\x -> x * x) list


-- 部分関数適用とカリー化
---------------------------------------------------------

-- Haskellでは, カリー化を用いて「複数の引数を取る1つの関数」を「1つの引数を取る複数の関数」に分割する.
-- これを実現するために使われるのが「部分適用」

    prod x y = x * y

-- この関数のシグネチャを見る
-- prod :: (Num a) => a -> a -> a
-- (Num a) => は, 以降の型記述でaはNumの型であるという宣言.

-- prod (2, 3)
-- をHaskellは以下の順序で実行する.
--   1. (prod 2) を実行して(\y -> 2 * y) を得る.
--   2. ((\y -> 2 * y) 3) を実行して6を得る.


-- 遅延評価
---------------------------------------------------------

    myRange start step = start:(myRange (start + step) step)

-- *Main> take 10 (myRange 1 3)
-- [1,4,7,10,13,16,19,22,25,28]

-- *Main> take 5 (map ((* 2) . (* 5)) [1..])
-- [10,20,30,40,50]

-- (* 5) は部分適用関数で, この関数になにか渡すと5倍されて返ってくる.


-- 型推論
---------------------------------------------------------

-- シェルで返る型が見えるオプションつける.
-- Prelude> :set +t

-- ユーザ独自のデータ型を追加するにはdataを使う
-- コンソールに表示できるようにderivingを付けておこう

    data Suit = Spades | Hearts deriving (Show)
    data Rank = Ten | Jack | Queen | King | Ace deriving (Show)
    type Card = (Rank, Suit) -- 組み合わせた別名を追加
    type Hand = [Card]

    value :: Rank -> Integer
    value Ten = 1
    value Jack = 2
    value Queen = 3
    value King = 4
    value Ace = 5

    cardValue :: Card -> Integer
    cardValue (rank, suit) = value rank

-- ポリモーフィック ... aは「なんでも」. なんかのリストを受け取り同じ型のリストを返す
    backwards :: [a] -> [a]
    backwards [] = []
    backwards (h:t) = backwards t ++ [h]

-- クラス
-- > クラスとは, 入力に応じて，どの演算が実行可能かを定義したものである．Clojureのプロトコルと同じだと考えてよい．
-- > クラスには, いくつかの関数シグニチャが定義されている. ある型がこれらすべての関数をサポートしていれば, その型は当該クラスのインスタンスである.
-- p.259

-- ある型がクラスXの関数シグニチャを満たしていれば，その型は「Xのインスタンス」となる.

    stagger :: (Num t) => t -> t
    stagger d = d + 2
    crawl d = d + 1

    treasureMap d =
        crawl (stagger (stagger d))

-- 実行される順に stagger -> stagger -> crawl, と書けないものか.
-- letを連続して書く方法もある.

    letTreasureMap (v, d) = let d1 = stagger d
                                d2 = stagger d1
                                d3 = crawl d2
                            in d3

-- これをさらに綺麗に書きたい, そこでモナドですよ.

-- > モナドを一言で表すと，特別なやり方で複数の関数を組み合わせるための方法だと言える．
-- p.261

-- モナドの構成要素
--   1. 型構成子: コンテナとなるものの型を変数に取る.
--   2. return: 関数をラップしてモナドに入れ込む.
--   3. >>=(バインド): 関数を数珠つなぎにする.

    data Position t = Position t deriving (Show)

    stagger2 (Position d) = Position (d + 2)
    crawl2 (Position d) = Position (d + 1)

    -- Haskell組込みモナドと衝突しないようにreturnの代わりにrtn, >>=の代わりに>>==を定義する
    rtn x = x
    x >>== f = f x -- 「対応する関数を, モナド内の値を引数として呼び出している」

    treasureMap2 pos = pos >>==
                      stagger2 >>==
                      stagger2 >>==
                      crawl2 >>==
                      rtn

-- モナドをくるむsyntax sugar, do.
    tryIo = do putStr "Enter your name: " ;
               line <- getLine ;
               let { backwards = reverse line } ;
               return ("Hello. Your name backwards is " ++ backwards)

-- *Main> tryIo
-- Enter your name: Hash
-- "Hello. Your name backwards is hsaH"

-- 命令形っぽく見えるが, 実際はモナド.
