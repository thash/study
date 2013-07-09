;; printlnをMultimethodなしで再実装する.
(defn my-print [ob]
  (.write *out* ob))

(defn my-println [ob]
  (my-print ob)
  (.write *out* "\n"))

;; これだとnilのときwriteできなくてエラる.
;; user=> (my-println nil)
;; NullPointerException   java.io.PrintWriter.write (PrintWriter.java:443)

;; nilを回避するだけでなく, 扱えるtypeをstring以外まで拡張しよう.
;; condでnil, seqなどいろんなものを分岐して出力方法を定義してやる(略)
;; こんな方法はやりたくないよね, と. そこでmultimethod.
;; => ch8/multimethod.clj
