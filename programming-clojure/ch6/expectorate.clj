(ns examples.glup
  (:import (java.io FileInputStream OutputStreamWriter BufferdWriter)))

(defn expectorate [dst content]
  (with-open [writer (-> dst
                         FileOutputStream.
                         OutputStreamWriter.
                         BufferdWriter.)]
    (.write writer (str content))))

