index
==================

* 7.1: Macroを使うとき従うべきルール
* 7.2: Macroを実際に使ってみる
* 7.3: 複雑になりがちなMacroをシンプルに
* 7.4: いろいろな種類のMacro


7.1 When to User Macros
=============================

2つのRuleと1つの例外がある.

* Rule(1): Macroを書くな.
* Rule(2): Macroを書いていいのは, Macroがパターンをカプセル化する唯一の方法であるときのみ.
* Exception: そのMacroで人生がラクになるなら書いていい.

ただし例外が適用出来るケースを知るまでには経験を積む必要がある.


7.2 Writing a Control Flow Macro
==================================

ifの代わりになるwhen-notというのがある(Rubyではunless)が, これをマクロで実装してみよう.
まず第一にfunctionとして実現できないか試してみる.

    (defn unless [expr form]
      (if expr nil form))

これ, exprがtrueの時もformが実行されて意味ない. Clojureにおいて関数の引数は正規評価される(渡される前に評価される)ためである.

> The problem is that Clojure evaluates all the arguments before passing them to a function

Macroなら引数をその場で評価しないのでこの問題を解決できる.

    (defmacro unless [expr form]
      (list 'if expr nil form))

macroexpand-1 や macroexpand で展開後の式が確認できる. macroexpand-1は1段階だけ展開するが, -1なしversionは展開後に現れたマクロもさらに展開する.
=> ch7/macros.clj


7.3 Making Macros Simpler
========================================

Macroのsyntax list.

https://dl.dropbox.com/s/odwhtd3x9tvy8ze/2013-06-30%20at%204.56.00%20PM.png

Quasiquote(準クォート)は `(form ~(hoge 1 2)) のように使うらしい. Gaucheでは`~`の代わりに`,`が使われていた.

=> ch7/making-macros-simpler.clj


7.4 Taxonomy of Macros
========================================

"special form" つまりifやrecurはfirst-classじゃない. 変数に入れたりapplyしたりできない.
が, それをmacroでwrapすることで, macro expansion timeにおいてはClojureのfeaturesはすべてfirst-classであると言って良い.
special formを生成するmacroは書くのが難しいが価値がある.

> Because macros do not evaluate their arguments

3回くらい出てきたのでこれ重要ぽい. で, この特性により制御構文も作れますよと. たとえばand.

    (defmacro and
      ([] true)
      ([x] x)
      ([x & rest]
        `(let [and# ~x]
         (if and# (and ~@rest) and#))))

再帰的にどんどんテストしていって, すべてpassすれば引数がなくなってtrueを返すと.


## Java Interop

> Clojure programs call into Java via the . (dot), new, and set! special forms.
> However, idiomatic Clojure code often uses macros such as .. (threaded member access) and doto to simplify forms that call Java.

Javaをcallするときは.やnew, set!などを使うけど, 理想的Clojureコードを目指すなら..やdotoを使うべき.


## Postpoing Evaluation

Clojureではほぼすべてのseqがlazyだけど, 明示的にlazyにすることもできる.
SICPでサンプルとして作ったまんまの名前, delayとforce.

    (delay & expr)
    (force expr)


## Wrapping Evaluation

なんかevaluationの前後をwrapしたMacro多いよね, という話.
timeはevaluationの処理時間を出力して, letやbindingはevaluationに特定の束縛を加える. with-openやdosyncも評価に何らかの意味を与えている.


## Avoiding Lambdas

歴史的理由から無名関数はlambdaと呼ばれる.
sometimes, macroはlambda呼び出しに書き換えることができる. が,

> the anonymous function approach requires more work on the part of the caller

なので使わない方がいい.

