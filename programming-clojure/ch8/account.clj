(ns examples.multimethods.account)
(alias 'acc 'examples.multimethods.account)

(defstruct account :id :tag :balance)

(defmulti interest-rate :tag)
(defmethod interest-rate ::acc/Checking [_] 0M)
(defmethod interest-rate ::acc/Savings [_] 0.05M)


(defmulti account-level :tag)
(defmethod account-level ::acc/Checking [acct]
  (if (>= (:balance acct) 5000) ::acc/Premium ::acc/Basic))
(defmethod account-level ::acc/Savings [acct]
  (if (>= (:balance acct) 1000) ::acc/Premium ::acc/Basic))

;; Basic or Premium, Checking or Savings という2通りの分岐, 計4patternを一発で定義できる.
;; defmulti定義時にclassでも:tag(個別field)でもなく, 無名関数によりunique性を作り出すのがキモ.
(defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
(defmethod service-charge [::acc/Basic ::acc/Checking]   [_] 25)
(defmethod service-charge [::acc/Basic ::acc/Savings]    [_] 10)
(defmethod service-charge [::acc/Premium ::acc/Checking] [_] 0)
(defmethod service-charge [::acc/Premium ::acc/Savings]  [_] 0)

(println (service-charge {:tag ::acc/Checking :balance 1000})) ;;=> 25
(println (service-charge {:tag ::acc/Savings :balance 1000}))  ;;=> 0

;; さらにDRY化できる.
;; Premiumなら0ってのがわかるので, CheckingとSavingを"derive"でまとめてやる.

(derive ::acc/Savings ::acc/Account)
(derive ::acc/Checking ::acc/Account)

;; こうなる.
(defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
(defmethod service-charge [::acc/Basic ::acc/Checking]   [_] 25)
(defmethod service-charge [::acc/Basic ::acc/Savings]    [_] 10)
;; (defmethod service-charge [::acc/Premium ::acc/Checking] [_] 0)
;; (defmethod service-charge [::acc/Premium ::acc/Savings]  [_] 0)
(defmethod service-charge [::acc/Premium ::acc/Account]  [_] 0)

;; いろんなopen source project見てmultimethodの使われ方見るといいよーと.

