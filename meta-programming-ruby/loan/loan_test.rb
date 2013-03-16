# -*- encoding: UTF-8 -*-
require 'test/unit'
require './loan'

class FakeTime
  def self.now; 'Mon Apr 06 12:15:50'; end
end

class TestLoan < Test::Unit::TestCase
  def test_conversion_to_string
    Loan.instance_eval { @time_class = FakeTime }
    loan = Loan.new('War and peace')
    assert_equal 'WAR AND PEACE loaned on Mon Apr 06 12:15:50', loan.to_s
  end
end

