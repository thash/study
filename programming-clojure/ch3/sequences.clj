(use '[clojure.java.io :only (reader)])
(defn non-blank? [line] (if (re-find #"\S" line) true false))

(defn non-svn? [file] (not (.contains (.toString file) ".svn")))
(defn clojure-source? [file] (.endsWith (.toString file) ".clj"))

(defn clojure-loc [base-file]
  (reduce
    +
    (for [file (file-seq base-file)
          :when (and (clojure-source? file) (non-svn? file))]
      (with-open [rdr (reader file)]
        (count (filter non-blank? (line-seq rdr)))))))

(println
  (clojure-loc (java.io.File. "/Users/hash/work/study/programming-clojure"))
) ;=> 210


(def song {:name "Agnus Dei"
           :artist "Krzysztof Pend"
           :album "Polish Requiem"
           :genre "Classical"})

(println song)
(println (assoc song :kind "MPEG Audio File"))
(println (dissoc song :genre))
(println (select-keys song [:name :artist]))
(println (merge song {:size 222244, :time 507245}))

;; 非破壊的. song変更なし
(println song)

;;sets
#{"java" "c" "d" "clojure"}
;; intersection, unicon, difference, select etc
;; join, project, etc
