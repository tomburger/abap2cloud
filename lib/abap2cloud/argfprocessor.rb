class ArgfProcessor
  def each
    ARGF.readlines.each do |line|
      yield line.chomp
    end
  end
end
