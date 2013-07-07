Java Down and Dirty
============================

Clojure is powerful & lean. ... lean?

> It’s lean, in that it can get right to the metal.

なんやろ. まあいいや. 以下ClojureとJavaの関係がいくつか説明される.

* Clojureは"直接"(= Javaのコードを介さず)JVM実行可能なバイナリコードに変換される.
* ClojureはJavaのライブラリを"直接"(= Lisp的にwrapせず)呼び出す.
  * でもまあfunctional styleにwrapするのも自由っちゃ自由だよ.
* Clojureでは明示的なerror handlingは不要(!?)
  * with-open idiomがresouece利用を助けてくれるから楽になるみたい.


9.1 Exception Handling
=========================

いちおうJavaのtry/catch/finally/throw的な関数も使えるんだけど,
以下の理由からほとんど使わない.

* Clojureではchecked exceptionを扱う必要がない
  * checked exceptionとは:
* (本来finallyで必要になる)resource cleanupはwith-openなどのマクロで隠蔽されている

ソースを見てみた. 最後10行くらい見ると確かにclojureのtry/throw/finallyを使っている.

    user=> (doc with-open)
    -------------------------
    clojure.core/with-open
    ([bindings & body])
    Macro
      bindings => [name init ...]

      Evaluates body in a try expression with names bound to the values
      of the inits, and a finally clause that calls (.close name) on each
      name in reverse order.

    user=> (source with-open)
    (defmacro with-open
      [bindings & body]
      (assert-args
         (vector? bindings) "a vector for its binding"
         (even? (count bindings)) "an even number of forms in binding vector")
      (cond
        (= (count bindings) 0) `(do ~@body)
        (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                                  (try
                                    (with-open ~(subvec bindings 2) ~@body)
                                    (finally
                                      (. ~(bindings 0) close))))
        :else (throw (IllegalArgumentException.
                       "with-open only allows Symbols in bindings"))))


type hinting == 型ヒント != 型推論
type hintingで実行のパフォーマンスが上がる.


9.2 Wrestling with the Integers
==========================================

Integerのoperatorには3つの種類がある.
unchecked, default, promoting.

unchecked: 速いがoverflowをチェックしないので間違った結果を返す可能性がある. 注意して使う.
default: 常にoverflow checkを行う
promote: automatically promote from primitives to big numbers on overflow.
         勝手に型を変更(promote = 昇進)してくれるのかな


9.3 Optimizing for Performance
========================================

パフォーマンス測るときはtime1発じゃなくてdotimesで何回か出してみるのが良い.

    (defn sum-to [n] (loop [i 1 sum 0]
      (if (<= i n) (recur (inc i) (+ i sum)) sum)))

    user=> (dotimes [_ 5] (time (sum-to 1000)))
    "Elapsed time: 0.165 msecs"
    "Elapsed time: 0.795 msecs"
    "Elapsed time: 0.753 msecs"
    "Elapsed time: 0.487 msecs"
    "Elapsed time: 0.488 msecs"

引数と返り値に型ヒントをつけてやると早くなる.

    (defn ^long integer-sum-to [^long n] (loop [i 1 sum 0]
      (if (<= i n) (recur (inc i) (+ i sum)) sum)))

    user=> (dotimes [_ 5] (time (integer-sum-to 1000)))
    "Elapsed time: 0.116 msecs"
    "Elapsed time: 0.121 msecs"
    "Elapsed time: 0.087 msecs"
    "Elapsed time: 0.083 msecs"
    "Elapsed time: 0.088 msecs"

ほんとだ. 次はadd(+)をuncheckedなやつにしてみる

    (defn ^long unchecked-sum-to [^long n] (loop [i 1 sum 0]
      (if (<= i n) (recur (inc i) (unchecked-add i sum)) sum)))

    user=> (dotimes [_ 5] (time (unchecked-sum-to 1000)))
    "Elapsed time: 0.103 msecs"
    "Elapsed time: 0.066 msecs"
    "Elapsed time: 0.056 msecs"
    "Elapsed time: 0.054 msecs"
    "Elapsed time: 0.052 msecs"

型ヒントほどは大きくないが速くなった(型ヒント外してuncheckedのみ試したがたしかに型ヒントと比較して効果は薄い).


    (defn better-sum-to [n]
      (reduce + (range 1 (inc n))))

    user=> (dotimes [_ 5] (time (better-sum-to 1000)))
    "Elapsed time: 1.352 msecs"
    "Elapsed time: 0.969 msecs"
    "Elapsed time: 1.186 msecs"
    "Elapsed time: 0.833 msecs"
    "Elapsed time: 1.083 msecs"

おっそｗｗｗｗｗ betterとはｗ

    (defn best-sum-to [n]
      (/ (* n (inc n)) 2))

    user=> (dotimes [_ 5] (time (best-sum-to 1000)))
    "Elapsed time: 0.097 msecs"
    "Elapsed time: 0.005 msecs"
    "Elapsed time: 0.015 msecs"
    "Elapsed time: 0.004 msecs"
    "Elapsed time: 0.011 msecs"

これがいちばん速い. 適切なアルゴリズム選ぶほうが圧倒的に効果が高い.
ちなみにさらに型ヒンをトつけると平均0.005くらいになった.


## Adding type hints

* ref
  * http://tnoda-clojure.tumblr.com/post/50280501176/type-hinting-and-primitive-hinting
  * http://tnoda-clojure.tumblr.com/post/36446730922/veiled-reflection-when-using-multidim-arrays
  * http://d.hatena.ne.jp/athos/20110416/apply_ctor

リフレクションの警告を出すように変えてやって, describe-classを定義してみる.

    user=> (set! *warn-on-reflection* true)
    true

    user=>     (defn describe-class [c]
      #_=>       {:name (.getName c)
      #_=>        :final (java.lang.reflect.Modifier/isFinal (.getModifiers c)})
    Reflection warning, NO_SOURCE_PATH:2:14 - reference to field getName can't be resolved.
    Reflection warning, NO_SOURCE_PATH:3:51 - reference to field getModifiers can't be resolved.

> リフレクション (reflection) とは、プログラムの実行過程でプログラム自身の構造を読み取ったり書き換えたりする技術のことである。

リフレクションを使うと

* Pros
  * コードの中で動的に決定できる部分が増える
* Cons
  * コンパイル時エラーが出ない(実行するまでエラーがわからない)

プログラムを動的な方向に倒す機能...  Rubyだと klass = Object.const_get('Foo'); klass.new となる感じ. Javaではver.6からサポート. でも遅いらしくこんな記述も.

> 速い Clojure の大敵といえば Java リフレクションです．*warn-on-reflection* を true にしておくと，REPL でリフレクションを警告してくれるので便利です．
> http://tnoda-clojure.tumblr.com/post/36446730922/veiled-reflection-when-using-multidim-arrays

つーか, 上にも出た型ヒントの大きな効果の一つはリフレクションを速くしてくれるところらしい.
describe-classの引数を [^Class c] とすると警告が出なくなる.


9.4 Creating Java Classes in Clojure
================================================

Clojureのobjectは*すべて*何らかのJava Interfaceをimplementしてる. functionはRunnableとCallableをimplementしてるとか.

proxy. interfaceをgenerateする方法, とのこと.

    user=> (doc proxy)
    -------------------------
    clojure.core/proxy
    ([class-and-interfaces args & fs])
    Macro
      class-and-interfaces - a vector of class names

      args - a (possibly empty) vector of arguments to the superclass
      constructor.

      f => (name [params*] body) or
      (name ([params*] body) ([params+] body) ...)

      Expands to code which creates a instance of a proxy class that
      implements the named class/interface(s) by calling the supplied
      fns. A single class, if provided, must be first. If not provided it
      defaults to Object.

      The interfaces names must be valid interface types. If a method fn
      is not provided for a class method, the superclass methd will be
      called. If a method fn is not provided for an interface method, an
      UnsupportedOperationException will be thrown should it be
      called. Method fns are closures and can capture the environment in
      which proxy is called. Each method fn takes an additional implicit
      first arg, which is bound to 'this. Note that while method fns can
      be provided to override protected methods, they have no other access
      to protected members, nor to super, as these capabilities cannot be
      proxied.


JavaのArrayはInterfaceを使っていないのでClojure(とJavaも)はそれらを特別に扱う.

    (make-array String 5)
    ;; => #<String[] [java.lang.String;@xxxxx]>
    (seq (make-array String 5))
    ;; => (nil nil nil nil nil)

seqでwrapするが吉.


