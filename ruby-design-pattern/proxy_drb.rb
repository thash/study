# encoding: utf-8
require 'drb/drb'

class MathService
  def add(a,b)
    return a + b
  end
end

math_service = MathService.new
DRb.start_service("druby://localhost:3030", math_service)
DRb.thread.join

DRb.start_service
