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

### 間違いの例
# 1. global variables ($hoge) はsingletonではない
#    変数なので $hoge = 'Bad'とすれば書き換えられてしまう.
# 2. 変更を避けるためにグローバル定数使ってもだめ.
#    遅延が使えない(1も), singleを保証できない(2個目を作れてしまう)

# ClassそのものをSingletonとして使うことも出来る. instanceが複数できないことは保証できる.
# => "instance化されないclass"ならModuleとして定義してしまうのもあり

module ModuleBasedLogger
  ERROR = 1
  WARNING = 2
  INFO = 3
  @@log = File.open("logm.txt", "w")
  @@level = WARNING

  def self.error(msg)
    @@log.puts(msg)
    @@log.flush
  end
  # (ry
end

ModuleBasedLogger.error('heyhey')

# Rubyの柔軟性とSingleton
# 結局苦労してsingletonにしてもcloneすれば複製できてしまう
# 言語として完全に禁止することはできない.


### Singleton pattern使用上の注意
#
# * Singleton != global variables
# * そもそも想定ケースでSingletonが必要ない
# * 他のClassがSingletonのsingleton性に依存してしまう
#   以下実例.

class DatabaseConnectionManager
  include Singleton

  def get_connection
    # return connection
  end
end

class PreferenceManagerXXX
  def initialize
    @reader = PrefReader.new
    @writer = PrefWriter.new
    @preferences = { :display_splash => false, :background_color => :blue }
  end

  def save_preferences
    preference = {}
    @writer.write(@preferences)
  end

  def get_preferences
    @preferences = @reader.read
  end

  class PrefWriter
    def write(preferences)
      connection = DatabaseConnectionManager.instance.get_connection
      # write preferences info
    end
  end

  class PrefReader
    def read
      connection = DatabaseConnectionManager.instance.get_connection
      # read and return preferences information
    end
  end
end

# better
class PreferenceManager
  def initialize
    @reader = PrefReader.new
    @writer = PrefWriter.new
    @preferences = { :display_splash => false, :background_color => :blue }
  end

  def save_preferences
    preferences = {}
    @writer.write(DatabaseConnectionManager.instance, @preferences)
  end

  def get_preferences
    @preferences = @reader.read(DatabaseConnectionManager.instance)
  end
end

