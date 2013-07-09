(ns noir-clojurebreaker.models.game)
(defn create []
  (vec (repeatedly 4 (fn [] (rand-nth ["r" "g" "b" "y"])))))



;; copy from clojurebreaker/src/clojurebreaker/game.clj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '[clojure.math.combinatorics :as comb])

(defn exact-matches
  "Given two collections, return the number of positions where
   the collections contain equal items."
  [c1 c2]
  (let [[_ _ matches] (diff c1 c2)]
    (count (remove nil? matches))))

;; diffって3種出て来るのか...
;; user=> (diff [:r :g :g :b] [:r :y :y :b])
;; [[nil :g :g] [nil :y :y] [:r nil nil :b]]

(defn unordered-mathces
  "Given two collections, return a map where each key is an item
  in both collections, and each value is the number of times the
  value occurs in the collection with fewest occurrences."
  [c1 c2]
  (let [f1 (select-keys (frequencies c1) c2)
        f2 (select-keys (frequencies c2) c1)]
    (merge-with min f1 f2)))

;; user=> (unordered-mathces [:r :g :g :b] [:y :y :y :g])
;; {:g 1}
;; user=> (frequencies [:r :g :g :b])
;; {:r 1, :g 2, :b 1}


(defn score [c1 c2]
  (let [exact (exact-matches c1 c2)
        unordered (apply + (vals (unordered-mathces c1 c2)))]
    {:exact exact :unordered (- unordered exact)}))

;; user=> (score [:r :g :g :b] [:r :y :y :g])
;; {:exact 1, :unordered 1}


;; 10.2 Testing the Scorer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-turn-inputs [colors n]
  (-> (comb/selections colors n)
      (comb/selections 2)))

;; user=> (comb/selections [:r :g :b] 2)
;; ((:r :r) (:r :g) (:r :b) (:g :r) (:g :g) (:g :b) (:b :r) (:b :g) (:b :b))

;; user=> (-> (comb/selections [:r :g :b] 2) (comb/selections 2))
;; (((:r :r) (:r :r)) ((:r :r) (:r :g)) ((:r :r) (:r :b)) ((:r :r) (:g :r)) ((:r :r) (:g :g)) ((:r :r) (:g :b)) ((:r :r) (:b :r)) ((:r :r) (:b :g)) ((:r :r) (:b :b)) ((:r :g) (:r :r)) ((:r :g) (:r :g)) ... ((:b :b) (:b :b))) 計81ペア

(defn score-inputs
  "Given a sequence of turn inputs, return a lazy sequence of
   maps with :secret, :guess, and :score."
  [inputs]
  (map
    (fn [[secret guess]]
      {:secret (seq secret)
       :guess (seq guess)
       :score (score secret guess)})
    inputs))

;; これで65535通りのsecret/guessペアが生成される.
;; (->> (generate-turn-inputs [:r :g :b] 2)
;;      (score-inputs))

