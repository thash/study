;; glup.clj, expectorate.clj はFileだけにしか使えないので, 抽象化する.

;; (defn make-reader [src]
;;   (-> src FileInputStream. InputStreamReader. BufferedReader.))
;;
;; (defn make-writer [dst]
;;   (-> dst FileOutputStream. OutputStreamWriter. BufferdWriter.))


(defn gulp [src]
  (let [sb (StringBuilder.)]
    (with-open [reader (make-reader src)]
      (loop [c (.read reader)]
        (if (neg? c)
          (str sb)
          (do
            (.append sb (char c))
            (recur (.read reader))))))))

(defn expectorate [dst content]
  (with-open [writer (make-writer dst)]
    (.write writer (str content))))

;; update make-reader, make-writer
(defn make-reader [src]
  (-> (condp = (type src)
        java.io.InputStrem src
        java.lang.String (FileInputStream. src)
        java.io.File (FileInputStream. src)
        java.net.Socket (.getInputStream src)
        java.net.URL (if (= "file" (.getProtocol src))
                       (-> src .getPath FileInputStream.)
                       (.openStream src)))
    InputStreamReader.
    BufferedReader.))

(defn make-writer [dst]
  (-> (condp = (type dst)
        java.io.OutputStream dst
        java.io.File (FileOutputStream. dst)
        java.lang.String (FileOutputStream. dst)
        java.net.Socket (.getOutputStream dst)
        java.net.URL (if (= "file" (.getProtocol dst))
                       (-> dst .getPath FileOutputStream.)
                       (throw (IllegalArgumentException.
                                "Can't write to non-file URL"))))
    OutputStreamWriter.
    BufferdWriter.))

;; これで一応複数のタイプに対応したコードがかけたが, このままでは拡張性に乏しい.
;; ユーザが扱えるtypeを増やそうとしたら, make-reader, make-writerのコードを書き換えないといけない.

;;; JavaのInterface ;;;





