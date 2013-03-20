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

  @@instance = SimpleLogger.new
  def self.instance
    return @@instance
  end

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

  private_class_method :new
end

begin
  logger = SimpleLogger.new
  logger.level = SimpleLogger::INFO

  loggerX = SimpleLogger.new
  p logger.object_id; p loggerX.object_id
  # return different object id
  # => 70124623647480
  # => 70124623647380
rescue NoMethodError
  p $!
  # => #<NoMethodError: private method `new' called for SimpleLogger:Class>
end

logger1 = SimpleLogger.instance
logger2 = SimpleLogger.instance
p logger1.object_id; p logger2.object_id
# exactly the same object
# => 70124623647520
# => 70124623647520

# use Ruby standard lib in the real world
# 上記の実装との違いが1点.
# 上記は必要になる前にSingleton instanceを作るeager instantiationで,
# Singleton moduleの実装は実際にinstance methodが呼ばれた時まで遅延するlazy instantiation.
require 'singleton'
class SimpleLoggerX
  include Singleton
end

