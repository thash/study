# encoding: utf-8
require 'forwardable'

class SimpleWriter
  def initialize(path)
    @file = File.open(path, 'w')
  end

  def write_line(line)
    @file.print(line)
    @file.print("\n")
  end

  def pos
    @file.pos
  end

  def rewind
    @file.rewind
  end

  def close
    @file.close
  end
end

class WriteDecorator
  extend Forwardable

  def_delegators :@real_writer, :write_line, :pos, :rewind, :close

  def initialize(real_writer)
    @real_writer = real_writer
  end
end

class NumberingWriter < WriteDecorator
  def initialize(real_writer)
    super(real_writer)
    @line_number = 1
  end

  def write_line(line)
    @real_writer.write_line("#{@line_number}: #{line}")
    @line_number += 1
  end
end

class CheckSumingWriter < WriteDecorator
  attr_reader :check_sum

  def initialize(real_writer)
    @real_writer = real_writer # superじゃない
    @check_sum = 0
  end

  def write_line(line)
    line.each_byte {|byte| @check_sum = (@check_sum + byte) % 256 }
    # @check_sum += "\n"[0] % 256
    #=> decorator.rb:70:in `+': String can't be coerced into Fixnum (TypeError)
    @real_writer.write_line(line)
  end
end

class TimeStampingWriter < WriteDecorator
  def write_line(line)
    @real_writer.write_line("#{Time.now}: #{line}")
  end
end

# 行番号をつけた後にTimestampを入れて最後にchecksumを取りたいとき

writer = CheckSumingWriter.new(
           TimeStampingWriter.new(
             NumberingWriter.new(
               SimpleWriter.new('final.txt'))))

p writer
writer.write_line("Hello out there")
p writer.check_sum


# Dynamic Decorator -- Strength of Ruby
w = SimpleWriter.new('out')

# open Singleton class
class << w
  alias old_write_line write_line
  def write_line(line)
    old_write_line("#{Time.now}: #{line}")
  end
end

# Decorator by module
module TimeStampingWriter2
  def write_line(line)
    super("#{Time.new}: #{line}")
  end
end

module NumberingWriter2
  attr_reader :line_number
  def write_line(line)
    @line_number ||= 1
    super("#{@line_number}: #{line}")
    @line_number += 1
  end
end

ww = SimpleWriter.new('filee')
ww.extend(NumberingWriter2)
ww.extend(TimeStampingWriter2)
ww.write_line('helllllllllo')

