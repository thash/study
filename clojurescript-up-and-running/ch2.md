Hello World
=======================================

leiningenと, "lein-cljsbuild" を使う.
lein-cljsbuildはplugin. project.clj に

    :dependencies [[org.clojure/clojure "1.5.1"]
                   [org.clojure/clojurescript "0.0-1806"]]
    :plugins [[lein-cljsbuild "0.3.2"]]

を追加. cljsのreplを起動するには

    $ lein trampoline cljsbuild repl-rhino

こうする. なげー
rhino: JVM上で動作するヘッドレスな(= viewを描画しない?) JavaScriptエンジンだそうな.
ただrhinoのreplはコンソール限定. DOMにアクセスしてゴニョゴニョするREPLはch9参照のこと.


ClojureScriptをJavaScriptにコンパイルする
============================================

まずディレクトリを整える. 標準のlein projectを微妙に修正.

    - hello-world/
      - README.md
      - project.clj
      - resources/
        - public/ ;; <============ Compile後のJSを保存する場所
      - src/
        - clj/
          - hello_world/
            - core.clj
        - cljs/ ;; <======== 2種類以上のソースがある場合はsrc/以下を2個に分ける

project.cljにsource-paths, output-to(コンパイル済JSが格納される場所)を設定.
本文のは書式が古いらしく, leinが以下のように修正してくれた.

  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs"],
     :compiler
     {:pretty-print true,
      :output-to "resources/public/hello.js",
      :optimizations :whitespace}}]}

で次のようなのをsrc/cljs/以下に書く.

    (ns hello-world.hello)
    (.write js/document "<p>Hello, world!</p>")

js/という名前空間をおもむろに指定できる模様.
Javaの関数を使う感覚で.writeを使い, document.writeを表す.

ようやくコンパイル.

    $ lein cljsbuild once

resources/public/hello.js ができる. が, 20,000行くらいあってわろた.
ClojureScriptランタイムとコアライブラリが含まれているらしい.
once => autoに変えると, 変更を検知して自動ビルドしてくれる.

あとはHTMLをresource/public/index.htmlなどで作成, 生成されたJSを読み込む.


ここでオンラインのGetting Startedをやる
=============================================

https://github.com/clojure/clojurescript
