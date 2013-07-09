(println (true? true)) ;=> true
(let [[_ _ z] [1 2 3]] (println z)) ;=> 3

(defrecord Book [title author])
(println (->Book "tittttt" "aauuu")) ;=> #user.Book{:title tittttt, :author aauuu}
;; (:title book) ; => tittttt

;; you can read document by executing (doc greeting) in REPL
(defn greeting
  "Returns a greeting of the form 'Hello, username.'"
  [username]
  (str "Hello, " username))

(println (greeting "Hash")) ;=> Hello, Hash

(defn greeting
  "Returns a greeting of the form 'Hello, username.'"
  ([] (greeting "World"))
  ([username] (str "Hello, " username)))

(println (greeting)) ;=> Hello, World

;;; Namespaces ;;;
; (resolve 'foo)
; (in-ns 'hhhooo) ; switch namespace

;; import => only for Java classes.
(import '(java.io InputStream File))
;; require => clojure classes. aliases can also be defined.
(require 'clojure.string) ;=> clojure.string/split
(require '[clojure.string :as s]) ;=> s/split


;;; Calling Java ;;;
(def rnd (new java.util.Random))
(println (. rnd nextInt))
;; dot(.) special form:
;;   (. class-or-instance methodname & args)
;; (methodname & args) can be surrounded by parenthesis.


;;; Recur ;;;
(println
  (loop [result [] x 5]
    (if (zero? x)
      result
      (recur (conj result x) (dec x))))
) ;=> [5 4 3 2 1]


(defn indexed [coll] (map-indexed vector coll))
(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

;; Clojure's "for" is not a loop but a sequence comprehension.
;; -- 内包記法 for.
;;     user=> (for [x (range 1 10)] (* x 2))
;;     (2 4 6 8 10 12 14 16 18)
;;     user=> (for [x (range 10 21) :when (and (not= x 13) (not= x 15) (not= x 19))] x)
;;     (10 11 12 14 16 17 18 20)
;; :when, :while, or :let are available.

;; Clojureのsetは"setの要素を含むかどうか"をテストするfunctionである. どういうことかというと
;;     user=> (index-filter #{\a \c} "abcdefg")
;;     (0 2)


;;; Functional vs Imperative ;;;
;; index-of-any
(defn index-of-any [pred coll]
  (first (index-filter pred coll)))

;; indexOfAny(Java): http://commons.apache.org/proper/commons-lang/javadocs/api-2.6/org/apache/commons/lang/StringUtils.html#indexOfAny(java.lang.String, char[)]

;; index-of-anyはfunctionalであり, imperative(命令式)の書き方に比べて
;;   simple, 比較回数が少ない, 余計な変数を必要としない, 拡張性に富む
;; などのメリットがある.
;; simpleさの実例として, imperative verでは
;;   マッチしない時はmagic number -1を返すべきか? nilか? 特別なsymbolか?
;; ということに悩むかもしれないが, functional verには悩みそのものが存在しない.
;; また, indexOfAnyは文字列にしか使えないがindex-of-anyは文字列に限らず使える (拡張性).


;;; 2.8 Metadata ;;;
;; Clojure本体に隠されたメタデータたち. 例:
;;    user=> (keys (meta #'str))
;;    (:arglists :ns :name :column :added :static :doc :line :file :tag)

;; 自前でmetadataを追加するには
;;     ^metadata form
;; という書式に則る.
(defn ^{:tag String} shout
  [^{:tag String} s] (.toUpperCase s))
;; defn直後のtag Stringは返り値の型, []内は引数の型.
;; metadataは(引数+bodyを()で囲ってやりさえすれば)最後においてもいい. 読みやすくなる.
(defn shout
  ([^{:tag String} s] (.toUpperCase s))
  ^{:tag String})
;; :tagでクラス名を表すのは慣例であり, 慣例すぎて
;;     ^String
;; と書いてもいいらしい.
(defn shout
  ([^String s] (.toUpperCase s))
  ^String )
