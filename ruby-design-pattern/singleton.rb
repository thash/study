# encoding: utf-8
class ClassVariableTester
  @@class_count = 0

  def initialize
    @instance_count = 0
  end

  def increment
    @@class_count = @@class_count + 1
    @instance_count = @instance_count + 1
  end

  def to_s
    "class_count: #{@@class_count}, instance_count: #{@instance_count}"
  end

end

c1 = ClassVariableTester.new
c1.increment
c1.increment
p c1.to_s # => "class_count: 2, instance_count: 2"

c2 = ClassVariableTester.new
p c2.to_s # => "class_count: 2, instance_count: 0"

class SimpleLogger
  attr_accessor :level

  ERROR = 1
  WARNING = 2
  INFO = 3

  def initialize
    @log = File.open("log.txt", "w")
    @level = WARNING
  end

  def error(msg)
    @log.puts(msg)
    @log.flush
  end

  def warning(msg)
    @log.puts(msg) if @level >= WARNING
    @log.flush
  end

  def info(msg)
    @log.puts(msg) if @level >= INFO
    @log.flush
  end
end

logger = SimpleLogger.new
logger.level = SimpleLogger::INFO


