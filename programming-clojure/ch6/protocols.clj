;; Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; > One piece of Clojure’s solution to the expression problem is the protocol.


;; glup.clj, expectorate.clj はFileだけにしか使えないので, 抽象化する.
;; (defn make-reader [src]
;;   (-> src FileInputStream. InputStreamReader. BufferedReader.))
;;
;; (defn make-writer [dst]
;;   (-> dst FileOutputStream. OutputStreamWriter. BufferedWriter.))


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
    BufferedWriter.))

;; これで一応複数のタイプに対応したコードがかけたが, このままでは拡張性に乏しい.
;; ユーザが扱えるtypeを増やそうとしたら, make-reader, make-writerのコードを書き換えないといけない.

;;; JavaのInterface ;;;
(definterface IOFactory
  (^java.io.BufferReader make-reader [this])
  (^java.io.BufferedWriter make-writer [this]))


;;; Protocol ;;;
;; Expressino Problem http://lambda-the-ultimate.org/node/2232 と,
;; Clojureの解答 http://www.ibm.com/developerworks/java/library/j-clojure-protocols/

;; Interfaceと比較したProtocolのメリットは
;; * Existing datatypes can be extended to implement new interface with no modification to the datatype.
;; * Protocol method names are namespaced, so there is no risk of name collision when multiple parties choose to extend the same extant type.

;; 新たに定義する場合
(defprotocol IOFactory
  "A protocol for things that can be read from and written to."
  (make-reader [this] "Creates a BufferedReader.")
  (make-writer [this] "Creates a BufferedWriter."))

;; 既存のtypeをextendする場合

(extend InputStream
  IOFactory
  {:make-reader (fn [src]
                  (-> src InputStreamReader. BufferedReader.))
   :make-writer (fn [dst]
                  (throw (IllegalArgumentException.
                           "Can't open as an InputStream.")))})
(extend OutputStream
  IOFactory
  {:make-reader (fn [src]
                  (throw (IllegalArgumentException.
                           "Can't open as an OutputStream.")))
   :make-writer (fn [dst]
                  (-> dst OutputStreamWriter. BufferedWriter.))})

;; ダサい...
;; 他に, java.io.Fileをextend-typeする方法もある.
(extend-type File
  IOFactory
  (make-reader [src]
    (make-reader (FileInputStream. src)))
  (make-writer [dst]
    (make-writer (FileInputStream. dst))))
;; こっちのがマシだ

;; で, 一度定義したprotocolはextend-protocolで拡張できる.
(extend-protocol IOFactory
  Socket
  (make-reader [src]
    (make-reader (.getInputStream src)))
  (make-writer [dst]
    (make-writer (.getOutputStream dst)))
  URL
  (make-reader [src]
    (make-reader
      (if (= "file" (.getProtocol src))
        (-> src .getPath FileInputStream.)
        (.openStream src))))
  (make-writer [dsr]
    (make-writer
      (if (= "file" (.getProtocol dst))
        (-> dst .getPath FileInputStream.)
        (throw (IllegalArgumentException.
                 "Can't write to non-file URL"))))))

