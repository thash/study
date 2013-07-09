;; > 2. Trampoling a mutual recursion.
;; clojure.core/trampoline
;; ([f] [f & args])
;;   trampoline can be used to convert algorithms requiring mutual
;;   recursion without stack consumption. Calls f with supplied args, if
;;   any. If f returns a fn, calls that fn with no arguments, and
;;   continues to repeat, until the return value is not a fn, then
;;   returns that non-fn value. Note that if you want to return a fn as a
;;   final value, you must wrap it in some data structure and unpack it
;;   after trampoline returns.

(println (trampoline + 1 2)) ;=> 3

(defn trampoline-fibo [n]
  (let [fib (fn fib [f-2 f-1 current]
              (let [f (+ f-2 f-1)]
                (if (= n current)
                  f
                  #(fib f-1 f (inc current)))))]
    (cond
      (= n 0) 0
      (= n 1) 1
      :else (fib 0N 1 2))))

(println (trampoline trampoline-fibo 10)) ;=> 55N

;; even, oddを相互依存のtrampoline定義する
(declare my-odd? my-even?) ;; 未定義エラーを出さないために宣言だけする.
(defn my-odd? [n]
  (println (str "my-odd?: " (.toString n)))
  (if (= n 0)
    false
    #(my-even? (dec n))))

(defn my-even? [n]
  (println (str "my-even?: " (.toString n)))
  (if (= n 0)
    true
    #(my-odd? (dec n))))

(println (trampoline my-even? 18))
; my-even?: 18
; my-odd?: 17
; ...
; my-even?: 2
; my-odd?: 1
; my-even?: 0
; true

