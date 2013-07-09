(ns examples.functional)

;; stackを無駄に使うfibonacci
(defn stack-consuming-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (stack-consuming-fibo (- n 1))
             (stack-consuming-fibo (- n 2)))))

(println (stack-consuming-fibo 10)) ;=> 55

(defn tail-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (fib next (+ current next) (dec n))))] ;; (dec n) は (- n 1) .
    (fib 0N 1N n))) ;; 0N, 1Nとは?

(println (tail-fibo 10)) ;=> 55N

(defn recur-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (recur next (+ current next) (dec n))))]
    (fib 0N 1N n)))

(println (recur-fibo 10)) ;=> 55N

;; lazy-sequence
;; 無限に続くfibonacci数列を生成
(defn lazy-seq-fibo
  ([]
   (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
       (cons n (lazy-seq-fibo b n))))))

(println (take 11 (lazy-seq-fibo))) ;=> (0 1 1N 2N 3N 5N 8N 13N 21N 34N 55N)

;; 明示的にlazy-seqを使わなくてもこう書ける. 暗黙のlazy.
(println (take 11 (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N]))))
;=> (0N 1N 1N 2N 3N 5N 8N 13N 21N 34N 55N)
;=> ちなみに map first なしだと
;  ([0N 1N] [1N 1N] [1N 2N] [2N 3N] [3N 5N] [5N 8N] [8N 13N] [13N 21N] [21N 34N] [34N 55N] [55N 89N])

(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))
(def lots-of-fibs (take 10000000 (fibo)))

(println (nth lots-of-fibs 100)) ;; nthでアクセスすれば大量の計算が必要ない.
;=> 354224848179261915075N
;; 一方, (take 100000 (fibo))をそのまま実行すると全部realizeしようとして爆発
;; "sequenceそのもの"ではなく, "sequenceを返す関数"として定義していることに注意.


;; 再帰的定義
(def head-fibo (lazy-cat [0N 1N] (map + head-fibo (rest head-fibo))))
(println (take 11 head-fibo)) ;; takeする分にはok
;; (nth head-fibo 100000) これをやるとOutOfmemoryError.
;; > "The problem is taht the top-level var head-fibo holds the head of the collection"
;; 必要ない場合はheadをkeepしないように注意


;; loop + recurを使って再帰.
;; 2回連続でコインの表(head)が出た数を数える
(defn count-heads-pairs [coll]
  (loop [cnt 0 coll coll] ;; loopの度にcollを上書き, cntは0に.
    (if (empty? coll)
      cnt
      (recur (if (= :h (first coll) (second coll))
               (inc cnt)
               cnt)
             (rest coll)))))

(println (count-heads-pairs [:h :h :h :t :h])) ;=> 2
(println (count-heads-pairs [:h :t :h :t :h])) ;=> 0

;; [:h :t :t :h :h :h]
;; を2個ずつ取っていく関数 by-pairsを定義して利用.
(defn by-pairs [coll]
  (let [take-pair (fn [c]
                    (when (next c) (take 2 c)))]
    (lazy-seq
      (when-let [pair (seq (take-pair coll))]
        (cons pair (by-pairs (rest coll)))))))

(println (by-pairs [:h :t :t :h :h :h]))
;=> ((:h :t) (:t :t) (:t :h) (:h :h) (:h :h))

(defn count-heads-pairs2 [coll]
  (count (filter (fn [pair] (every? #(= :h %) pair))
                 (by-pairs coll))))
(println (count-heads-pairs2 [:h :h :h :t :h])) ;=> 2
;; recurでループさせるより "expressive" だよね, と.

;; で, この実はClojureはpartitionというもっと一般的な関数を用意している.
(println (partition 2 [:h :t :t :h :h :h]))
;=> ((:h :t) (:t :h) (:h :h)) おっと. デフォルトで重複しないように取ってくれる.
;; 同じにするにはstepを指定すれば良い.
(println (partition 2 1 [:h :t :t :h :h :h]))


;; def絡みの亜種.
;; "defonce" は存在しない時だけ定義するdefの亜種.
;; "defn-" はprivateな(現在のnamespace内でのみ使える)関数を定義.


;;; Currying ;;;
;; > "When you curry a function, you get a new function that takes one argument and returns the original function with that one argument fixed."

;; partial: 関数の部分適用
;; (partial 関数f & partial-args) partialに渡す partial-args は, 関数f本来の引数以下である必要がある.

;; clojure.core/partial
;; ([f] [f arg1] [f arg1 arg2] [f arg1 arg2 arg3] [f arg1 arg2 arg3 & more])
;;   Takes a function f and fewer than the normal arguments to f, and
;;   returns a fn that takes a variable number of additional args. When
;;   called, the returned function calls f with args + additional args.

(defn faux-curry [& args] (apply partial partial args))

(def add-3 (partial + 3))
(println (add-3 7))

;; faux-curryは*本当のcurryではない*
(println ((faux-curry true?) (= 1 1)))
;=> #<core$partial$fn__445 clojure.core$partial$fn__445@683a51de>
;; mergeされた関数を返す. 本当のcurryはtrueを返さないといけないらしい.
;; Clojureには本当のcurryingはないわけだが, 実用上問題ない.
;; ほぼすべてのプログラマはcurryingとpartial applicationを同じものとして捉えている.


;; JVMの制約内でFunctional Programmingを実現するためのClojureの戦略.
;; 例: loop/recur
;; loop/recurはself-recursionには使えるが,
;; even?とodd?をお互いを使って定義するようなmutual-recursionには使えない.
;; ここで使える戦略4つ.
;;   1. Converting to self-recursion.
;;   2. Trampoling a mutual recursion.
;;     => ch4/trampoline.clj
;;   3. Replacing recursion with laziness.
;;   4. Shortcutting recursion with memorization.

;; Clojureで時間を測るにはtime.
