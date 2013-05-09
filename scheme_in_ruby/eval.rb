# encoding: utf-8

def _eval(exp, env)
  if not list?(exp)
    if immediate_val?(exp)
      exp
    else
      # lookup_primitive_fun(exp)
      lookup_var(exp, env)
    end
  else
    if special_form?(exp)
      eval_special_form(exp, env)
    else
      fun = _eval(car(exp), env)
      args = eval_list(cdr(exp), env)
      apply(fun, args)
    end
  end
end

def list?(exp)
  exp.is_a?(Array)
end

def lookup_primitive_fun(exp)
  $primitive_fun_env[exp] # Global env!?
end

$primitive_fun_env = {
  :+ => [:prim, lambda{|x, y| x + y}],
  :- => [:prim, lambda{|x, y| x - y}],
  :* => [:prim, lambda{|x, y| x * y}],
}

# 初期環境にprimitive_funを設定しておくことで,
# 組み込み関数を変数と同じようにlookup_varで扱うことが出来る.
$global_env = [$primitive_fun_env]

def car(list)
  list[0]
end

def cdr(list)
  list[1..-1]
end

def eval_list(exp, env)
  exp.map{|e| _eval(e, env)}
end

def immediate_val?(exp)
  num?(exp)
end

def num?(exp)
  exp.is_a?(Numeric)
end

def apply(fun, args)
  if primitive_fun?(fun)
    apply_primitive_fun(fun, args)
  else
    lambda_apply(fun, args)
  end
end

def primitive_fun?(exp)
  exp[0] == :prim
end

def apply_primitive_fun(fun, args)
  fun_val = fun[1]
  fun_val.call(*args)
end

# run it
# puts _eval([:+, 1, 2]) # => 3

# var: valの対応をHashで表している. 探索をfindで代用.
def lookup_var(var, env)
  alist = env.find{|alist| alist.key?(var)}
  if alist == nil
    raise "couldn't find value to variables: '#{var}'"
  end
  alist[var]
end

# 変数をkey, 値をvalとしたHashを"env(= Array)"のアタマに追加する
def extend_env(parameters, args, env)
  alist = parameters.zip(args)
  h = Hash.new
  alist.each {|k,v| h[k] = v}
  [h] + env
end

## let
def eval_let(exp, env)
  parameters, args, body = let_to_parameters_args_body(exp)
  # ここの+argsがキモ. letで宣言した変数束縛を追加している
  new_exp = [[:lambda, parameters, body]] + args
  _eval(new_exp, env)
end

def let_to_parameters_args_body(exp)
  [exp[1].map{|e| e[0]}, exp[1].map{|e| e[1]}, exp[2]]
end

def let?(exp)
  exp[0] == :let
end

## lambda
def eval_lambda(exp, env)
  make_closure(exp, env)
end

def make_closure(exp, env)
  parameters, body = exp[1], exp[2]
  [:closure, parameters, body, env]
end

def lambda_apply(closure, args)
  parameters, body, env = closure_to_paramegers_body_env(closure)
  new_exp = extend_env(parameters, args, env)
  _eval(body, new_exp)
end

def closure_to_paramegers_body_env(closure)
  [closure[1], closure[2], closure[3]]
end

def lambda?(exp)
  exp[0] == :lambda
end

def special_form?(exp)
  lambda?(exp) or
    let?(exp)
end

def eval_special_form(exp, env)
  if lambda?(exp)
    eval_lambda(exp, env)
  elsif let?(exp)
    eval_let(exp, env)
  end
end
