require 'minitest/spec'
require 'minitest/autorun'

module Functional

  def apply(enum)
    enum.map &self
  end
  alias | apply

  def reduce(enum)
    enum.inject &self
  end
  alias <= reduce

  def compose(f)
    if self.respond_to?(:arity) && self.arity == 1
      lambda {|*args| self[f[*args]]}
    else
      lambda {|*args| self[*f[*args]]}
    end
  end
  alias * compose

end

class Proc; include Functional; end
class Method; include Functional; end
class Symbol; include Functional; end

=begin
### Examples ###
sum = lambda {|x,y| x+y }
mean = (sum<=a)/a.size
deviation = lambda {|x| x - mean }
square = lambda {|x| x * x }
sd = Math.sqrt((sum <= square | (deviation | a)) / (a.size - 1))

# use compose
sd = Math.sqrt((sum <= square * deviation | a) / (a.size - 1))
=end
