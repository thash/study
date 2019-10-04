class Quote
  def initialize
    @str = "The quick brown fox..."
  end

  def di
    puts @str
  end

  define_method :display do
    puts @str
  end
end

q = Quote.new
q.di # => The quick brown fox...
q.display # => The quick brown fox...
