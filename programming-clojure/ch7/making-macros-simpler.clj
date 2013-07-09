;; 7.3 Making Macros Simpler
;; ============================================

;; いくつかの書き方.
;; まずこれはconcat, list, list...のあたりが読みにくい.
(defmacro chain
  ([x form] (list '. x form))
  ([x form & more] (concat (list 'chain (list '. x form)) more)))

;; そこでunquoteを使う. GaucheではQuasiquoteと呼ばれて`と,で実現していた.
(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form)) ~more))

;; さらに微妙に改良. moreは任意長引数なんで, argsに対する*argsみたいなもんが必要. ~@である.
(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form)) ~@more))

;; uniqueなlocal nameを生成する # の使いドコロ.
;; これ自動でやってくれないのは何故だろう?
(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
    {:result result# :elapsed (- (System/nanoTime) start#)}))

(println (bench (str "a" "b")))
;;=> {:result ab, :elapsed 28000}

;; # ... Auto-gensym の効果.
user=> (macroexpand-1 '(bench (str "a" "b")))
(clojure.core/let [start__141__auto__ (java.lang.System/nanoTime)
                   result__142__auto__ (str "a" "b")]
  {:result result__142__auto__,
   :elapsed (clojure.core/- (java.lang.System/nanoTime) start__141__auto__)})

(defmacro bench2 [expr]
  `(let [start (System/nanoTime)
         result ~expr]
     {:result result :elapsed (- (System/nanoTime) start)}))

user=> (macroexpand-1 '(bench2 (str "a" "b")))
(clojure.core/let [user/start (java.lang.System/nanoTime)
                   user/result (str "a" "b")]
  {:result user/result,
   :elapsed (clojure.core/- (java.lang.System/nanoTime) user/start)})

