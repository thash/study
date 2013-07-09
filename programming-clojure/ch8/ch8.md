Multimethods
=======================

ClojureのMultimethodsはJavaのpolymorphismに似ているが, より一般的なものである.
* Java methodを読んだ時, typeとsingle objectに応じて実行される処理を決定する

    (defmulti hoge class)      ;; classで振り分けることを宣言
    (defmethod m1 String [arg] ;; 判別にはStringを使ってもらう
      (...))

condで分岐する代わりにdefmethod宣言を分ける.
オリジナルに手を入れることなく新しい実装を追加できる.

さらに, default動作も指定できる(condでやるならelseに入る).


8.4 Creating Ad Hoc Taxonomies
======================================

:はkeyword. ::は現在のnsでのkeywordを表す.

    user=> :Checking
    :Checking
    user=> ::Checking
    :user/Checking

=> ch8/account.clj


8.4 When Should I Use Multimethods?
=============================================

"dispatch"とよく出てくる節.

コード例としてclojure.test, clojure.inspectorが出てくる.
テストライブラリ覚えておこう. clojure.test.

* typeに応じて分岐するならmultimethodを使おう.
  * ここでtypeとはJava Classやdata tagとは限らず, whatever you discover them to be である.

もしわかんなかったらmultimethod版/cond分岐版と書いてみて読みやすい方を選ぶべしとのこと.

