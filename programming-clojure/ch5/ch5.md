## Ref

以下2つの関数は同じ挙動をするが, alter(参照と更新を同時に行う)の方が読みやすい.

    user=> (defn naive-add-message [msg]
      #_=>   (dosync (ref-set messages (cons msg @messages))))
    user=> (defn add-message [msg]
      #_=>   (dosync (alter messages conj msg)))

ちなみにconsとconjの違いは単に引数の順序.
(cons item sequence)
(conj sequence item)

alterと同じ形の引数をとるcommuteも存在する

    user=> (defn add-message-commute [msg]
      #_=>   (dosync (commute messages conj msg)))

使い分けのさじ加減, 実例が書いてくれてるけどイメージが追いつかなかった.
まあalter使っとけよくらい...?


## Vars

thread-localな変数とはどういうことか. おもむろにbindingの説明.

(def ^:dynamic foo 10)
(let [foo 9999] (println foo)) ;;=> 9999
(binding [foo 9999] (println foo)) ;;=> 9999

(defn print-foo [] (println foo))
(let [foo 9999] (print-foo)) ;;=> 10
(binding [foo 9999] (print-foo)) ;;=> 9999

関数による束縛スコープを上書きするbinding. "root binding"を隠す.

ふつうにdefするとdynamic varじゃないので怒られる

user=> (def foo 10)
user=> (binding [foo 9999] (print-foo))
IllegalStateException Can't dynamically bind non-dynamic var: user/foo  clojure.lang.Var.pushThreadBindings (Var.java:353)

クロージャclosureの束縛を上書きすることで何が嬉しくなるんだろうか. 不要な複雑さではないか

> dynamic binding has great power. But...(ry Functions that use dynamic bindings are not pure functions and can quickly lose the benefits of Clojure's functional style. (p.130)

と書いてるからまあ危険性理解して使えよという感じか.


### Acting at a Distance

special variablesと呼ばれる*in*, *out*, *err*などはthread-wideなdynamic binding.

以下, bindingの使い方例
遅い計算を扱うとき, memoizeが使える. memorizeではない.

clojure.core/memoize
([f])
  Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use.

(defn calls-slow-double []
  (map slow-double [1 2 1 2 1 2]))
(time (dorun (calls-slow-double))) ;;=> "Elapsed time: 604.769 msecs"

(defn demo-memoize []
  (time
    (dorun
      (binding [slow-double (memoize slow-double)]
        (calls-slow-double)))))

(demo-memoize) ;;=> "Elapsed time: 202.358 msecs"


あと, ほぼ使わないけどset!もあるそうな.

