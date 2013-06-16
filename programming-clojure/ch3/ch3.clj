;; sequence (seq) というひとつの抽象化で様々なデータ構造を表現する.
;;
;; cons => cons
;; car  => first
;; cdr  => rest
;;
;; (seq coll) ;; Returns a seq on the collection.
;;     user=> (seq "heyhey")
;;     (\h \e \y \h \e \y)
;;     user=> (rest (seq "heyhey"))
;;     (\e \y \h \e \y)
;;     user=> (next "heyhey") ;; same as rest + seq
;;     (\e \y \h \e \y)

;; car, cdrの語源は acronyms that refer to implementation details of Lisp on the
;; Original IBM 704.

;;; Everyting is a Sequence ;;;
(println (first {:fname "Arton" :lname "Badra"}))
(println (rest {:fname "Arton" :lname "Badra"}))
;; REPLでやるとlistっぽく出力するけど型はseq.

;; sets
(println (first #{:the :quick :brown :fox}))

(println (sorted-set :the :quick :brown :fox))
(println (sorted-map :the :quick :brown :fox))

;; conj -- collectionに"要素"を入れる.
(println (conj '(1 2 3) 4)) ;=> (4 1 2 3)
(println (conj '(1 2 3) '(4 5 6))) ;=> ((4 5 6) 1 2 3)
; (println (conj 0 '(1 2 3) '(4 5 6))) ; => Error

;; into -- collectionを1つに.
(println (into '(1 2 3) '(4 5 6))) ;=> (6 5 4 1 2 3)
;; 順序が直感に反する. 内部で(4 5 6).each {|elem| cons elem baselist } みたいな畳み込みをやっている

(println (class '(rest (1 2 3)))) ;=> clojure.lang.PersistentList

;; Clojure sequenceはimmutable, 非破壊的. -> safe for concurrent access.


;; creating sequence
(println (range 10)) ;=> (0 1 2 3 4 5 6 7 8 9)
(println (range 4 10)) ;=> (4 5 6 7 8 9)
(println (repeat 4 2)) ;=> (2 2 2 2)
;; iterate は無限リストを生成する.
;; clojure.core/iterate
;; ([f x])
;;   Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects
(println (take 10 (iterate inc 2))) ;=> (2 3 4 5 6 7 8 9 10 11)
(println (interpose "|" ["hoge" "huga" "foo"])) ;=> (hoge | huga | foo)

;; list, vector, hash-set, hash-mapを作るには... そのまんまの関数が使える
(println (list [1 2 3])) ;=> ([1 2 3])
(println (hash-set [1 2 3])) ;=> #{[1 2 3]}
(println (hash-map :key1 'val1, 'key2 :val2, [:compound :key] nil))
;=> {[:compound :key] nil, :key1 val1, key2 :val2}

;; なんかが出てくるまで取得するtake-while <=> drop-while
(println (take-while (complement #{\a\e\i\o\u}) "tglnbsijaleg")) ;=> (t g l n b s)
(println (drop-while (complement #{\a\e\i\o\u}) "tglnbsijaleg")) ;=> (i j a l e g)

;; seqの全てが条件を満たすかどうか調べるにはevery? <=> some, not-every? <=> not-any?
(println (every? odd? [1 3 5]))
(println (some even? [1 2 3]))

(println (reduce * (range 1 10)))

(println "---------")
;; 強制評価 force は doall, dorun.
;; dorunはメモリに展開せずに値を捨てて走査していく(故に返り値はnil)
(def x (for [i (range 1 3)] (do (println i) i)))
(doall x)
(dorun x)


;; JavaのCollectionもいい感じにSeq化してくれるよーと.

;;; Regular Expression ;;;
(let [m (re-matcher #"\w+" "the quick brown fox")]
  (loop [match (re-find m)]
    (when match
      (println match)
      (recur (re-find m)))))

;; これをさらに高レベルに抽象化したものがre-seq.
(println (re-seq #"\w+" "the quick brown fox")) ;=> (the quick brown fox)

(import '(java.io File))
;; mapを使うなら勝手にseq化してくれるので,
(println (map #(.getName %) (seq (.listFiles (File. ".")))))
;; は
(println (map #(.getName %) (.listFiles (File. "."))))
;; こう書ける


