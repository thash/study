# encoding: utf-8

class CompositeBase
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def self.member_of(composite_name)
    attr_name = "parent_#{composite_name}"

    # ちょっとした防御
    raise 'Method redefinition' if instance_methods.include?(attr_name)
    code = %Q{
      attr_accessor :#{attr_name}
    }
    class_eval(code)
  end

  def self.composite_of(composite_name)
    member_of composite_name

    code = %Q{
      def sub_#{composite_name}s
        @sub_#{composite_name}s = [] unless @sub_#{composite_name}s
        @sub_#{composite_name}s
      end

      def add_sub_#{composite_name}(child)
        return if sub_#{composite_name}s.include?(child)
        sub_#{composite_name}s << child
        child.parent_#{composite_name} = self
      end

      def delete_sub_#{composite_name}(child)
        return unless sub_#{composite_name}s.include(child)
        sub_#{composite_name}s.delete(child)
        child.parent_#{composite_name} = nil
      end
    }
    class_eval(code)
  end

end

# メタプロなら自前のattr_readerを作るのも簡単
class Object
  def self.readable_attribute(name)
    code = %Q{
      def #{name}
        @#{name}
      end
    }
    class_eval(code)
  end
end

