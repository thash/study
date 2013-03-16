# encoding: utf-8
require './base'

# first,
# adapter.rb:13:in `encrypt': undefined method `^' for "h":String (NoMethodError)
# => define String#^

class String
  def ^(other)
    b1 = self.unpack("U*")
    b2 = other.unpack("U*")
    longest = [b1.length, b2.length].max
    b1 = [0]*(longest-b1.length) + b1
    b2 = [0]*(longest-b2.length) + b2
    b1.zip(b2).map{|a,b| a^b }.pack("U*")
  end
end

class Encrypter
  def initialize(key)
    @key = key
  end

  def encrypt(reader, writer)
    key_index = 0
    while not reader.eof?
      clear_char = reader.getc
      encrypted_char = clear_char ^ @key[key_index]
      writer.putc(encrypted_char)
      key_index = (key_index + 1) % @key.size
    end
  end

end

reader = File.open('message.txt')
writer = File.open('message.encrypted.txt', 'w')
encrypter = Encrypter.new('my secret key')
encrypter.encrypt(reader, writer)


class StringIOAdapter
  def initialize(string)
    @string = string
    @position = 0
  end

  def getc
    if @position >= @string.length
      raise EOFError
    end
    ch = @string[@position]
    @position += 1
    return ch
  end

  def eof?
    return @position >= @string.length
  end
end

encrypter = Encrypter.new('XYZZY')
reader = StringIOAdapter.new('We attack at dawn')
writer = File.open('out.txt', 'w')
encrypter.encrypt(reader, writer)
