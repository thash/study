ClojureScriptを使う理由
====================================

JavaScriptは圧倒的に勝利したが, 言語として欠陥があることには変わりなかった.
JavaScriptをWebのアセンブリ言語みたいなものと捉え, JavaScriptに変換する言語が幾つか開発されている.
CoffeeScriptは意図的にJavaScriptに似せている(欠陥をプログラマから隠している).

ClojureScriptはClojureの亜流であり, JavaScriptにコンパイルできる言語.
ClojureScriptはClojureの構文をJavaScriptにかぶせただけのものでは"ない".
不変データ, Lazy Seq, 第一級関数やマクロなども使えるサブセット(たぶん)である.
ClojureがJavaのすべてを覆い隠さないのと同じく, ClojureScriptも必要に応じてナマJavaScriptを使える.

