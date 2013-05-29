require './eval'

exp0 =
  [:letrec,
    [[:fact,
      [:lambda, [:n], [:if, [:<, :n, 1],
        1,
        [:*, :n, [:fact, [:-, :n, 1]]]]]]],
        [:fact, 3]]

exp1 =
  [:let,
    [[:fact,
      [:lambda, [:n], [:if, [:<, :n, 1],
        1,
        [:*, :n, [:fact, [:-, :n, 1]]]]]]],
        [:fact, 0]]

puts _eval(exp1, $global_env)

# 再帰しようとするとエラー
# /Users/hash/work/study/scheme_in_ruby/eval.rb:92:in `lookup_var': couldn't find value to variables: 'fact' (RuntimeError)
#   from /Users/hash/work/study/scheme_in_ruby/eval.rb:9:in `_eval'
#   from /Users/hash/work/study/scheme_in_ruby/eval.rb:15:in `_eval'
#   ...
# 理由 => letでfactをクロージャに束縛しているわけだが, このクロージャ内にfact自身は存在しないため.

# 解決策:
# https://www.evernote.com/shard/s11/sh/e797c200-51c1-4704-9384-97c5d439d379/c91e1c57a7d7b46f868b0c9a7bbd043d

puts _eval(exp0, $global_env)

# -- ここまでで純粋関数型言語に必要な要素は全て学んだ.
# 以下は純粋関数型言語のメリット
#   * 状態がないため, 変数をそれを束縛している値で単純に置き換えてよい
#     * SICPで言う"置き換えモデル"
#   * 関数呼び出しを減らすことが出来る(<-なぜ? 関数の一部は置き換え可能だから?)
#     * => 関数呼び出し時に必要な環境スタック(一時保存)が不要になり低コスト.
#   * どの順序で評価しても値が変わらないため並列に処理できる

