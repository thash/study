(defmulti my-print class) ;;
(defmethod my-print String [s]
  (.write *out* s))
(defmethod my-print nil [s]
  (.write *out* s))
;; => #<MultiFn clojure.lang.MultiFn@39494ff0>

;; default
(defmethod my-print :default [s]
  (.write *out* "#<")
  (.write *out* (.toString s))
  (.write *out* ">"))
;; user=> (my-print (java.sql.Date. 0))
;; #<1970-01-01>nil

;; :defaultが定義済だけど自分のコード中でオレオレdefaultを定義したいときは
(defmulti my-print class :default :mydefault)
;; みたいにできる


