(def x 100)
(println x) ;=> 100
(let [x 20] (println x)) ;=> 20
(println x) ;=> 100
; (binding [x 3] x)
;=> Exception in thread "main" java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: user/x

(println "---------")
(def ^:dynamic y 100)
(println y) ;=> 100
(let [y 20] (println y)) ;=> 20
(println y) ;=> 100
(binding [y 3] (println y)) ;=> 3
(println y) ;=> 100

;; letとbindingはやっていることが違う.
