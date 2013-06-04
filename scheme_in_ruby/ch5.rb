# 破壊的代入
def eval_set!(exp, env)
  var, val = setq_to_var_val(exp)
  var_ref = lookup_var_ref(var, env)
  if var_ref != nil
    var_ref[var] = _eval(val, env)
  else
    raise "undefined variable: '#{var}'"
  end
  nil
end

def setq_to_var_val(exp)
  [exp[1], exp[2]]
end

def setq?(exp)
  exp[0] == :setq
end
