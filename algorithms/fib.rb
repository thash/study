def no_memoized_fib(n)
  return 1 if n <= 2
  no_memoized_fib(n - 1) + no_memoized_fib(n - 2)
end
# puts no_memoized_fib(ARGV[0].to_i)

## no-memoized version
# $ time ruby fib.rb 40
# 102334155
# ruby fib.rb 40 9.39s user 0.03s system 99% cpu 9.421 total


@mem = {}
def memoized_fib(n)
  return 1 if n <= 2
  @mem[n] ||= memoized_fib(n - 1) + memoized_fib(n - 2)
end
# puts memoized_fib(ARGV[0].to_i)

### memoized version
# $ time ruby fib.rb 40
# 102334155
# ruby fib.rb 40 0.05s user 0.03s system 96% cpu 0.077 total

### 1000 ok
# $ time ruby fib.rb 1000
# ruby fib.rb 1000  0.05s user 0.03s system 92% cpu 0.082 total

### 10k order -> stack level too deep
# $ time ruby fib.rb 10000
# fib.rb:4: stack level too deep (SystemStackError)
# ruby fib.rb 10000  0.05s user 0.03s system 96% cpu 0.079 total


# 末尾再帰最適化?
# http://en.wikipedia.org/wiki/Tail_call

# http://stackoverflow.com/questions/824562/does-ruby-perform-tail-call-optimization
# を見て
#     RubyVM::InstructionSequence.compile_option = {
#       :tailcall_optimization => true,
#       :trace_instruction => false
#     }
# としてみたが効果なし


# というか再帰型から反復型へ書き換えればいいのか

# Fibonacciに関してperfect squareという判定法を発見．
# 大きな数でも効率的にそれがFibonacci数かどうか判定できるらしい．面白そう
# https://coderwall.com/p/guasua/how-to-find-if-a-number-is-a-fibonacci



# 閑話休題．反復で書こう．RosettaCodeからのコピペを改修．RosettaCode面白い
# http://rosettacode.org/wiki/Fibonacci_sequence
def fib_iter(n)
  return 1 if n <= 2
  prev, current = 1, 1
  (n - 2).times do
    ## 要はこう変数を更新したいんだが
    # tmp = current
    # current += prev
    # prev = tmp

    # tmpを使わず多重代入で一気に入れ替える
    prev, current = current, current + prev
  end
  current
end
# p (1..20).map{|i| fib_iter(i) }
# puts fib_iter(ARGV[0].to_i)


# $ time ruby fib.rb 10000
# ruby fib.rb 10000  0.06s user 0.03s system 94% cpu 0.090 total

# $ time ruby fib.rb 100000
# ruby fib.rb 100000  0.24s user 0.12s system 93% cpu 0.386 total

# $ time ruby fib.rb 1000000
# ruby fib.rb 1000000  13.82s user 7.39s system 99% cpu 21.237 total

# 反復すごい，10万まで一瞬，100万でも13秒かかったが帰ってきた...
# 先のRosettaCodeにも
# > though recursive solutions are generally considered too slow and are mostly used as an exercise in recursion
# って書いてるな．

# 試してみる
# $ time ruby fib.rb 4000000
# ruby fib.rb 4000000  258.80s user 212.34s system 99% cpu 7:52.67 total
# 7分!! 比例(100万の4倍)ではない．実用的かどうかはともかく一応終わる


# 反復アルゴリズム，冗長な書き方
def dasa_fib(n)
  if n <= 2
    return 1
    else
      prev = 1
      current = 1
      while n > 2 do
        tmp = current
        current = prev + current
        prev = tmp
        n = n - 1
      end
      return current
  end
end

# ちょっとRuby的にした反復アルゴリズム
def rubyish_fib(n)
  return 1 if n <= 2
  prev, current = 1, 1
  3.upto(n) do # uptoで意味を明確に
    prev, current = current, prev + current
  end
  current
end

# p (1..20).map{|i| rubyish_fib(i) }
#
# p       rubyish_fib(1) # .to_s.length #=> 1
# p      rubyish_fib(10) # .to_s.length #=> 2
# p     rubyish_fib(100) # .to_s.length #=> 21
# p    rubyish_fib(1000) # .to_s.length #=> 209
# p   rubyish_fib(10000) # .to_s.length #=> 2090
# p  rubyish_fib(100000) # .to_s.length #=> 20899
# p rubyish_fib(1000000).to_s.length #=> 208988

def sumup_even_fib(threshold)
  return 0 if threshold < 1
  return 2 if threshold == 1
  prev, current = 1, 1
  sum = prev
  print "#{prev}, "
  while current < threshold do
    print (current.even? ? "\"#{current}\"" : current), ', '
    sum += current if current.even?
    prev, current = current, prev + current
  end
  puts ''
  sum
end

puts sumup_even_fib(4000000)
# 1, 1, "2", 3, 5, "8", 13, 21, "34", 55, 89, "144", 233, 377, "610", 987, 1597, "2584", 4181, 6765, "10946", 17711, 28657, "46368", 75025, 121393, "196418", 317811, 514229, "832040", 1346269, 2178309, "3524578",
# 4613733
