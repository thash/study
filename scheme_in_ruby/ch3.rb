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

