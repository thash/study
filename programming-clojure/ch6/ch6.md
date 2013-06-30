概論
===========================

プログラムの再利用には抽象化が必須.
JavaはInterfaceを使って抽象化を実現した. ClojureでもdefinterfaceでJavaのInterfaceを作れはするんだが,
Clojure独自のProtocolという仕組みがある.

例として, Clojureの"slurp"と"spit"を再実装してみよう. これらはIO.read, IO.write的な感じか?


6.4 Datatypes
==========================

既存のtypeを拡張するのはprotocol.
一方, 新たなtypeを作るのがdatatypeである.

datatypeはclass, structure, fields(primitive可), mapなどを作れる. defaultでimmutable.

