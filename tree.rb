require 'singleton'

module Tree

  class Empty
    include Singleton
  end

  class Node
    attr_reader   :value, :left, :right
    attr_accessor :root
    def initialize(value=0, left: Empty.instance, right: Empty.instance)
      if [Node, Leaf, Empty].any?{|clz| left.is_a?(clz) } &&
          [Node, Leaf, Empty].any?{|clz| right.is_a?(clz) }
        @value = value
        @left  = left
        @right = right

        @root  = false
      else
        raise 'invalid branch(es)'
      end
    end
  end

  class Leaf
    attr_reader :value
    def initialize(value=nil)
      @value = value
    end
  end

end


# use sample tree at http://en.wikipedia.org/wiki/Binary_heap
left = Tree::Node.new(19, left:  Tree::Node.new(17,
                                                left:  Tree::Leaf.new(2),
                                                right: Tree::Leaf.new(7)),
                          right: Tree::Leaf.new(3))
right = Tree::Node.new(36, left: Tree::Leaf.new(25), right: Tree::Leaf.new(1))

p Tree::Node.new(100, left: left, right: right)


class BinaryHeap < Tree::Node

  def initialize(*args)
    super
    @root = true
  end

  def sort!
  end

  # display tree properly
  # def inspect
  # end

  # def insert(value)
  # end
end


