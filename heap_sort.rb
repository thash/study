require 'minitest/spec'
require 'minitest/autorun'

class Array
  def heap_sort
    heap = self.clone
    ((heap.size - 1) / 2).downto(0) do |i|
      heap = heapify(heap, i)
    end
    result = []
    heap.size.times do
      result << heap.shift
      heap = heapify(heap, 0)
    end
    result
  end

  def heapify(heap, largest)
    left  = (largest + 1) * 2 - 1
    right = (largest + 1) * 2
    max_index = largest
    max_index = left  if !heap[left].nil?  && heap[left]  < heap[max_index]
    max_index = right if !heap[right].nil? && heap[right] < heap[max_index]
    if largest != max_index # if changed
      heap[largest], heap[max_index] = heap[max_index], heap[largest] # swap
      heapify(heap, max_index)
    else
      heap
    end
  end
end

describe Array do
  it { [].respond_to?(:heap_sort).must_equal true }

  subject { ary }
  describe '[]' do let(:ary) { [] }; it { subject.heap_sort.must_equal [] } end
  describe '[1]' do let(:ary) { [1] }; it { subject.heap_sort.must_equal [1] } end
  describe '[1, 2]' do let(:ary) { [1, 2] }; it { subject.heap_sort.must_equal [1, 2] } end
  describe '[2, 1]' do let(:ary) { [2, 1] }; it { subject.heap_sort.must_equal [1, 2] } end
  describe '[2, 1, 3]' do let(:ary) { [2, 1, 3] }; it { subject.heap_sort.must_equal [1, 2, 3] } end
  describe '[3, 1, 2]' do let(:ary) { [3, 1, 2] }; it { subject.heap_sort.must_equal [1, 2, 3] } end
  describe '[2, 4, 1, 3]' do let(:ary) { [2, 4, 1, 3] }; it { subject.heap_sort.must_equal [1, 2, 3, 4] } end
  describe '[13, 1, 4, 10, 8, 7, 1, 16, 9, 12]' do
    let(:ary) { [13, 1, 4, 10, 8, 7, 1, 16, 9, 12] }
    it  { subject.heap_sort.must_equal [1, 1, 7, 8, 4, 9, 10, 12, 13, 16] }
  end
end
