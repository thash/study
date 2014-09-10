;; 文字`\a`
(= "abc" (str \a \b \c)) ; true

;; vector
(nth [:a :b :c] 1) ; :b

;; set
#{:x :y}

;; binding
(def hoge val)

;; 関数定義
(defn hoge [arg] (hoge arg))

;; 無名関数
(fn [arg] (hoge arg))

;; 関数適用apply. 以下は(f a b c)と等価
(apply f '(a b c))

;; * Schemeは"計算の要点をとらえた非情にコンパクトな言語を実現しようとした"
;; * CommonLispは"研究向けに使われているLispの様々な方言を標準化しようとした"
;; 一方でClojureは企業の開発者向けに汎用プログラミングのための実用的な道具として設計された.

;; 再帰
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; loopとrecur ... 無名再帰
;; recurを使わなければloopはletと同じ

(defn size [v]
  (if (empty? v)
      0
      (inc (size (rest v)))))

;; この名前付き再帰は次の無名再帰に書き換えられる

(defn size [v]
  (loop [l v, c 0]
    (if (empty? l)
        c
        (recur (rest l) (inc c)))))

;;;; 遅延評価
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(take 4 (repeat "hoge")) ; ("hoge" "hoge" "hoge" "hoge")

(->> [:lather :rinse :repeat] (cycle) (drop 2) (take 5))
;; (:repeat :lather :rinse :repeat :lather)


(take 20 (interleave (cycle (range 2)) (cycle (range 3))))
;; (0 0 1 1 0 2 1 0 0 1 1 2 0 0 1 1 0 2 1 0)

;; > JVMとは, 結局のところ, 型とインターフェイスだ
;; >
;; > p.213

;; recordとprotocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; オブジェクト指向の悪いところ(継承)を受け継がずに良い所(型とプロトコル(interface))を取り入れている.
;;
;;   * 型を定義するdefrecord
;;   * 型によって関数をグループ化するprotocol(JavaのInterface)
;;
;; 初期のClojureでは内部にJavaコードが多くあったが, それをClojureで実装し直したものが上記2機能である.

;; http://media.pragprog.com/titles/btlang/code/clojure/compass.clj
;; Compassプロトコルを定義. direction, left, rightを持つ
(defprotocol Compass
  (direction [c])
  (left [c])
  (right [c]))

(def directions [:north :east :south :west])
(defn turn
  [base amount]
  (rem (+ base amount) (count directions)))

;; recordを定義していく
(defrecord SimpleCompass [bearing]
  ;; Compassプロトコルの満たすべき関数を実装していく.
  Compass
  ;; 各関数の引数には本来インスタンスへの参照(this, self的なもの)が入るが, 使わないので_を書いて捨ててる
  (direction [_] (directions bearing))
  ;; Clojureでは値は不変. なので破壊的に変更せず
  ;; 新しいインスタンスを作って返す((Clazz. params)はnewに相当)
  (left [_] (SimpleCompass. (turn bearing 3)))
  (right [_] (SimpleCompass. (turn bearing 1)))
  Object
  (toString [this] (str "[" (direction this) "]")))


;; マクロ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro unless [test body]
  (list 'if (list 'not test) body))

(unless (= 1 2) 'wei) ; wei
(unless (= 1 1) 'wei) ; nil


;; 参照とトランザクションメモリ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; データベースはトランザクションによってデータの整合性を保証する
;;    * ロック
;;    * バージョニング

;; Javaなどの言語では, 共用リソースを破壊から守るためにロックを使う.
;; > ロックは基本的に, 並行制御のための作業をプログラマに負担させるものだ. しかしこの負担は耐え難いものになっている. 最近は特にその傾向が強い.
;; > p.219

;; ClojureはSTM(ソフトフェアトランザクショナルメモリ)という異なるアプローチを取る.
;; > この戦略は複数のバージョンを用いて一貫性と整合性を維持するものだ.
;; > p.219

;; -------- ref ---------
(def movie (ref "Star Wars"))

;; 参照
(deref movie) ; もしくは
@movie

;; alterはrefがderefされ(日本語), 第一引数として渡される
(alter movie str ": Episode 1")
;; これはエラーになる. 実際変更を行うときはトランザクションのスコープ内で行う
(dosync
 (alter movie str ": Episode 1"))

;; -------- atom ---------
(def danger (atom "Split at your own risk."))
;; 変更はreset!
(reset! danger "Split with impunity")
;; atomがvectorなんかだった時のために, 一部を変更するswap!もある.


;; その他の話題
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; * multimethodを使って, 複数のパラダイムに対応するコードが書ける
;; * Lispとしての側面だけでなく, Javaエコシステムへの理解も必要
;; * ClojureはLispとして優れている(これまでのLispで最高という意見も)
;;   * syntaxをゆるめカッコを減らした
;;   * JVMを基盤としたことで, Lisp系にありがちな貧弱なエコシステムという弱点を克服
;;   * リーダーマクロを避けClojureのsyntaxを制限したことで, 有害な分離方言を抑制した(わからん)
;;     * メタプログラミングのツールが失われるというデメリットでもある
;; * 並行性に対する有力なアプローチ
;; * 学習曲線がだいぶ辛い. 習得に時間かかる
;; * JVMで動作させるため末尾最適化が使えない. 不格好なloop/recur構文を使う必要があるという弱点

