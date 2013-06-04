require './eval'
# 関数定義の表現方法2つ
exp0 = [:define, :id, [:lambda, [:x], :x]]
exp1 = [:deifne, [:id, :x], :x]

# exp1はexp0の単なる糖衣構文ってやつ

puts _eval(exp0, $global_env)

exp2 =
  [:define, [:length, :list],
    [:if, [:null?, :list],
      0,
      [:+, [:length, [:cdr, :list]], 1]]]

exp3 = [:length, [:list, 1, 2]]

