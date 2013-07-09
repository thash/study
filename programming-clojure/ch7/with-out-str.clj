(defmacro with-out-str [& body]
  `(let [s# (new java.io.StringWrapper)]
    (binding [*out* s#]
      ~@body
      (str #s))))

(with-out-str (print "hello, ") (print @world))
