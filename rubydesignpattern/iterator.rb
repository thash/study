# encoding: utf-8

# external iterator
class ArrayIterator
  def initialize(array)
    @array = array
    @index = 0
  end
  def has_next?
    @index < @array.length
  end
  def item
    @array[@index]
  end
  def next_item
    value = @array[@index]
    @index += 1
    value
  end
end

i = ArrayIterator.new('abc')
p i
while i.has_next?
  p i.next_item
end

# inner iterator
p "--------------"

def for_each_element(array)
  i = 0
  while i < array.length
    yield(array[i])
    i += 1
  end
end

a = [10,20,30]
for_each_element(a) {|element| puts("The elem is #{element}")}

# Enumerable
# you only need: each, <=>

class Account
  attr_accessor :name, :balance

  def initialize(name, balance)
    @name = name
    @balance = balance
  end

  def <=>(other)
    balance <=> other.balance
  end

  class Portfolio
    include Enumerable

    def initialize
      @accounts = []
    end

    def each(&block)
      @accounts.each(&block)
    end

    def add_accoount(account)
      @accounts << account
    end
  end
end

# problems
class ChangeResistantArrayIterator
  def initialize(array)
    @array = Array.new(array)
    @index = 0
  end
end

def change_resistant_for_each_element(array)
  copy = Array.new(array)
  i = 0
  while i < copy.length
    yield(copy[i])
    i += 1
  end
end

# the book says "String class has method which scans each 'LINE'(not each chars) "
# => /Users/hash/work/books-tutorial/rubydesignpattern/iterator.rb:95:in `<main>': undefined method `each' for "aaaa\nbbb\nccc":String (NoMethodError)

require 'pathname'
pn = Pathname.new(`which ruby`.chomp)
pn.each_filename{|file| print file + ","}


# ObjectSpace!!
# ObjectSpace.each_object{|o| puts o}
# ObjectSpace.each_object(Numeric){|obj| puts obj}

def subclasses_of(superclass)
  subclasses = []
  ObjectSpace.each_object(Class) do |k|
    next if !k.ancestors.include?(superclass) || superclass == k ||
      k.to_s.include?('::') || subclasses.include?(k.to_s)
    subclasses << k.to_s
  end
  subclasses
end

p subclasses_of(Numeric)

