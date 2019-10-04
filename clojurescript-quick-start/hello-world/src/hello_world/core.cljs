(ns hello-world.core
  (:require react-dom))

(.render js/ReactDOM
  (.createElement js/React "h2" nil "Hello, React!")
  (.getElementById js/document "app"))

;; (println "Hello world!")
;; 
;; (defn average [a b]
;;   (/ (+ a b) 2.0)practitioner)
