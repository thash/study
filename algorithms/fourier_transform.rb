n = 10
coefficient = n
temp_re = Array.new(n, 0); temp_im = Array.new(n, 0)
(0..n).each do |i|
  (0..n).each do |j|
    puts "#{i},#{j}"
    temp_re[i] += re[j]
    temp_im[i]
  end
  temp_re[i]
  temp_im[i]
end
