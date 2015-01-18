# binary search
def search(pin, input)
  puts "searching #{pin} from #{input} ..."
  index_lo = 0
  index_hi = input.size - 1
  while index_lo <= index_hi
    middle = ((index_hi + index_lo) / 2.0).ceil
    if pin == input[middle] # found
      return "#{input.map.with_index {|n, i| i == middle ? n.to_s : n }}"
    elsif pin > input[middle]
      index_lo = middle + 1
    else # <=> "pin < input[middle]"
      index_hi = middle - 1
    end
  end
  return "NOT FOUND"
end

list = %w|1 3 5 11 12 13 17 22 25 28|.map(&:to_i)

puts search(25, list) # => [1, 3, 5, 11, 12, 13, 17, 22, "25", 28]
puts search(23, list) # => NOT FOUND
puts search( 5, list) # => [1, 3, "5", 11, 12, 13, 17, 22, 25, 28]


# ループを解体した場合の流れ
puts "list.size: #{list.size}"
index_lo = 0
index_hi = list.size - 1
p middle = ((index_hi + index_lo) / 2.0).ceil
p list[middle]

index_lo = middle # 更新. middle + 1のほうが無駄がない
p middle2 = ((index_hi + index_lo) / 2.0).ceil
p list[middle2]

index_lo = middle2
p middle3 = ((index_hi + index_lo) / 2.0).ceil
p list[middle3]
