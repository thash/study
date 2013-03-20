# encoding: utf-8
class Computer
  attr_accessor :display
  attr_accessor :motherboard
  attr_reader :drives

  def initialize(display=:ctr, motherboard=Motherboard.new, drives=[])
    @motherboard = motherboard
    @drives = drives
    @display = display
  end
end

class CPU
end

class BasicCPU < CPU
end

class TurboCPU < CPU
end

class Motherboard
  attr_accessor :cpu
  attr_accessor :memory_size

  def initialize(cpu=BasicCPU.new, memory_size=1000)
    @cpu = cpu
    @memory_size = memory_size
  end
end

class Drive
  attr_reader :type # hard_disk or cd or dvd
  attr_reader :size # in MB
  attr_reader :writable

  def initialize(type, size, writable)
    @type     = type
    @size     = size
    @writable = writable
  end
end

# 組み立てるのはめんどくさいよね
motherboard = Motherboard.new(TurboCPU.new, 4000)
drives = []
drives << Drive.new(:hard_disk, 200000, true)
drives << Drive.new(:cd, 760, true)
drives << Drive.new(:dvd, 4700, false)

computer = Computer.new(:lcd, motherboard, drives)
p computer
# => #<Computer:0x007f893b0ecdd0 @motherboard=#<Motherboard:0x007f893b0ece98 @cpu=#<TurboCPU:0x007f893b0ecec0>, @memory_size=4000>, @drives=[#<Drive:0x007f893b0ece48 @type=:hard_disk, @size=200000, @writable=true>, #<Drive:0x007f893b0ece20 @type=:cd, @size=760, @writable=true>, #<Drive:0x007f893b0ecdf8 @type=:dvd, @size=4700, @writable=false>], @display=:lcd>

# これを一発のnewで作るようなBuilderを定義する
class ComputerBuilder
  def initialize
    @computer = Computer.new
  end
  def turbo(has_turbo_cpu=true)
    @computer.motherboard.cpu = TurboCPU.new
  end
  def display=(display)
    @computer.display=display
  end
  def memory_size=(size_in_mb)
    @computer.motherboard.memory_size = size_in_mb
  end
  def add_cd(writer=false)
    @computer.drives << Drive.new(:cd, 760, writer)
  end
  def add_dvd(writer=false)
    @computer.drives << Drive.new(:dvd, 4000, writer)
  end
  def add_hard_disk(size_in_mb)
    @computer.drives << Drive.new(:hard_disk, size_in_mb, true)
  end
end

builder = ComputerBuilder.new
builder.turbo
builder.add_cd(true)
builder.add_dvd
builder.add_hard_disk(100000)

computer = builder.computer

