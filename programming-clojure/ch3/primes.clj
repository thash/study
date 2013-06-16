(ns examples.primes)
(def primes
  (concat
    [2 3 5 7]
    (lazy-seq
      (let [primes-from
            (fn primes-from [n [f & r]]
              (if (some #(zero? (rem n %))
                        (take-while #(<= (* % %) n) primes))
                (recur (+ n f) r)
                (lazy-seq (cons n (primes-from (+ n f) r)))))
            wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4 2
                          6 4 6 8 4 2 4 2 4 8 6 4 6 2  4 6
                          2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
        (primes-from 11 wheel)))))

(println
  (take 5 (drop 1000 (map vector (iterate inc 1) primes)))
)
;=> ([1001 7927] [1002 7933] [1003 7937] [1004 7949] [1005 7951])


;; 各関数の挙動を確認するためのメモ
(drop 5 (range 1 10)) ;=> (6 7 8 9)
(take 10 (iterate inc 1)) ;=> (1 2 3 4 5 6 7 8 9 10)
(take 10 (map vector (iterate inc 1))) ;=> ([1] [2] [3] [4] [5] [6] [7] [8] [9] [10])
(take 10 (map vector (iterate inc 1) [20 40 60 80])) ;=> ([1 20] [2 40] [3 60] [4 80])

