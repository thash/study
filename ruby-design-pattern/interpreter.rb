# encoding: utf-8

# インタープリタは通常2つの段階を含みます.
#  1. パーサがプログラムテキストを読み込み, 抽象構文木(AST: Abstract Syntax Tree)と呼ばれるデータ構造を作る
#  2. ASTを, 何らかの外部条件やコンテキストを背景にして評価する
# // ここで抽象構文木を直接書き下すのがLisp. Lispは(2)のプロセスだけでいい

# RubyでInterpreterを作りましょう

require 'find'

class Expression
end

class All < Expression
  def evaluate(dir)
    results = []
    Find.find(dir) do |p|
      next unless File.file?(p)
      results << p
    end
    results
  end
end

class FileName < Expression
  def initialize(pattern)
    @pattern = pattern
  end

  def evaluate(dir)
    results = []
    Find.find(dir) do |p|
      next unless File.file?(p)
      name = File.basename(p)
      results << p if File.fnmatch(@pattern, name)
    end
  end
end


### how to use
# expr_all = All.new
# files = expr_all.evaluate('test_dir')
#
# # mp3だけが欲しい場合
# expr_mp3 = FileName.new('*.mp3')
# mp3s = expr_mp3.evaluate('test_dir')

class Bigger < Expression
  def initialize(size)
    @size = size
  end

  def evaluate(dir)
    results = []
    Find.find(dir) do |p|
      next unless File.file?(p)
      results << p if (File.size(p) > @size)
    end
    results
  end
end

class Writable < Expression
  def evaluate(dir)
    results = []
    Find.find(dir) do |p|
      next unless File.file?(p)
      results << p if (File.writable?(p))
    end
    results
  end
end


# notやらandやら
class Not < Expression
  def initialize(expression)
    @expression = expression
  end

  def evaluate(dir)
    All.new.evaluate(dir) - @expression.evaluate(dir)
  end
end

### how to use
expr_not_writable = Not.new(Writable.new)
readonly_files = expr_not_writable.evaluate('test_dir')

small_expr = Not.new(Bigger.new(1024))
small_files = small_expr.evaluate('test_dir')

not_mp3_expr = Not.new(Filename.new('*.mp3'))
not_mp3 = not_mp3_expr.evaluate('test_dir')


class Or < Expression
  def initialize(expression1, expression2)
    @expression1 = expression1
    @expression2 = expression2
  end

  def evaluate(dir)
    result1 = @expression1.evaluate(dir)
    result2 = @Expression2.evaluate(dir)
    (result1 + result2)
  end
end

big_or_mp3_expr = Or.new(Bigger.new(1024),  FileName.new('*.mp3'))
big_or_mp3 = big_or_mp3_expr.evaluate('test_dir')


class And < Expression
  def initialize(expression1, expression2)
    @expression1 = expression1
    @expression2 = expression2
  end

  def evaluate(dir)
    result1 = @expression1.evaluate(dir)
    result2 = @Expression2.evaluate(dir)
    (result1 & result2)
  end
end

complex_expressionn = And.new(
                        And.new(Bigger.new(1024),
                                FileName.new('*.mp3')),
                      Not.new(Writable.new))

complex_expressionn.evaluate('test_dir')
complex_expressionn.evaluate('/tmp')

# GoFのInterpreter patterではASTがどこから来たかについて言及しない.
# 実際, 上記のようなちょっとしたファイル検索言語のパーサを作るのはそんなに難しくない.

# こんな構文を定義すると考える
#   and (and(bigger 1024) (filename *.mp3)) writable

class Parse
  def initialize(text)
    @tokens = text.scan(/\(|\)|[\w\.\*]+/)
  end

  def next_token
    @tokens.shift
  end

  def expression
    token = next_token

    if token == nil
      return nil

    elsif token == '('
      result = expression
      raise 'Expected )' unless next_token == ')'
      result

    elsif token == 'all'
      return All.new

    elsif token == 'writable'
      return Writable.new

    elsif token == 'bigger'
      return Bigger.new(next_token.to_i)

    elsif token == 'filename'
      return Filename.new(next_token)

    elsif token == 'not'
      return Not.new(expression)

    elsif token == 'and'
      return And.new(expression, expression)

    elsif token == 'or'
      return Or.new(expression, expression)

    else
      raise "Unexpected token: #{token}"
    end
  end
end

parser = Parser.new "add (and(bigger 1024) (filename *.mp3)) writable"
ast = parser.expression



## ParserのないInterpreter
class Expression
  def |(other)
    Or.new(self, other)
  end

  def &(other)
    And.new(self, other)
  end
end

# そろそろこんな感じに書けるようになってきた
#   (Bigger.new(2000) & Not.new(Writable.new) | Filename.new("*.mp3"))

# さらにsyntax sugarを...
def all
  All.new
end

def bigger(size)
  Bigger.new(size)
end

def name(pattern)
  FileName.new(pattern)
end

def except(expression) # Rubyのnotと衝突するので避ける
  Not.new(expression)
end

def writable
  Writable.new
end

# んで, こう書けるようになった
# (bigger(2000) & except(writable)) | file_name('*.mp3')


# XMLやYAMLなら既存のparserを使うことができる. これで独自言語を作るのはどうか?
# => 実際これらは言語表現に向いてない.
# => Racc(http://i.loveruby.net/ja/projects/racc/doc/usage.html)
#        (http://d.hatena.ne.jp/hs9587/20090131/1233373688)

# Interpreter patternはあまり実世界で適用されていないパターン. 実にもったいないこと.
# 独自言語を作るなら, (1)複雑になり過ぎないか? (2)誰がそれを使うのか? を考えよう

# Runt(http://runt.rubyforge.org/) -- Ruby template expression
#     (https://github.com/texel/runt)
#     日付に関する拡張の一例.
