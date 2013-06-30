(defmacro unless [expr form]
  (list 'if expr nil form))

(unless false (println "this should print"))
(unless true (println "this should NOT print"))

;; Using macroexpand-1, you can see the result of the macro expantion.
(println (macroexpand-1 '(unless false (println "this should print"))))
;;=> (if false nil (println this should print))

(println (macroexpand-1 '(.. arm getHand getFinger)))
;;=> (.. (. arm getHand) getFinger)
(println (macroexpand '(.. arm getHand getFinger)))
;;=> (. (. arm getHand) getFinger)


