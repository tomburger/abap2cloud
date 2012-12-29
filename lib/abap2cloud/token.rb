class Token
  
  DOT = :token_dot
  WORD = :token_word
  STRING = :token_string
  COLLON = :token_collon
  SLASH = :token_slash
  PUNCTION = :token_punction
  EQUAL = :token_equal
  
  attr_reader :kind, :value
  def initialize(kind, value='')
    @kind = kind
    @value = value
  end
  def to_s
    return @value.empty? ? @kind.to_s : "#{@kind} (#{@value})"
  end
end