# -*- encoding: UTF-8 -*-

module Kernel
def using(r)
  if block_given?
    begin
      yield
    ensure
      r.dispose
    end
    r.dispose
  else
    puts 'block kure><'
  end
end
end
