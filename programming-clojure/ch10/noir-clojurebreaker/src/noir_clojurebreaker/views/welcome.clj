(ns noir-clojurebreaker.views.welcome
  (:require [noir.session :as session]
            [noir-clojurebreaker.views.common :as common]
            [noir-clojurebreaker.models.game :as game])
  (:use [noir.core :only [defpage defpartial render]]
        [hiccup.form :only [form-to text-field submit-button]]))

;;; 名前が変わっててハマった...
;; hiccup.page-helpers => hiccup.page
;; hiccup.form-helpers => hiccup.form

;; (defpage "/" []
(defpage "/" {:as guesses}
         (when-not (session/get :game)
           (session/put! :game (game/create)))
         (common/layout (board)))

;;(defpartial board []
(defpartial board [{:keys [one two three four exact unordered]}]
  (when (and exact unordered)
    [:div "Exact: " exact " Unordered: " unordered])
  (form-to [:post "/guess"]
           (text-field "one")
           (text-field "two")
           (text-field "three")
           (text-field "four")
           (submit-button "Guess")))

(defpage [:post "/guess"] {:keys [one two three four]}
         (let [result (game/score (session/get :game) [one two three four])]
           (if (= (:exact result) 4)
             (do (session/remove! :game)
               (common/layout
                 [:h2 "Contratulations, you have solved the puzzle!"]
                 (form-to [:get "/"]
                          (submit-button "Start a new game"))))
             (do (session/flash-put! result) ;; Railsのflashのようなもの.
                 (render "/" {:one one
                              :two two
                              :three three
                              :four four
                              :exact (:exact result)
                              :unordered (:unordered result)})))))

