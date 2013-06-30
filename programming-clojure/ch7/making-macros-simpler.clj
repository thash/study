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

