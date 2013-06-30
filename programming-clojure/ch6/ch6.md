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

=> ch6/cryptovault.clj

6.5 Records
==========================

> One difference between records and maps is that, unlike maps, records are not functions of keywords.

    user=> (def mymap {:hoge 1 :fuga 2})
    user=> (mymap :hoge)
    1

    user=> (defrecord Note [pitch octave duration])
    user.Note
    user=> (def myrecord (->Note :D# 4 1/2))
    #'user/myrecord
    user=> (myrecord :pitch)
    ClassCastException user.Note cannot be cast to clojure.lang.IFn  user/eval169 (NO_SOURCE_FILE:1)

    user=> (:pitch myrecord)
    :D#

=> ch6/midi.clj

