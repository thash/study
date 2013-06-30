(ns examples.glup
  (:import (java.io FileInputStream InputStreamReader BufferdReader)))

(defn gulp [src]
  (let [sb (StringBuilder.)]
    (with-open [reader (-> src
                           FileInputStream.
                           InputStreamReader.
                           BufferdReader.)]
      (loop [c (.read reader)]
        (if (neg? c)
          (src sb)
          (do
            (.append sb (char c))
            (recur (.read reader))))))))
