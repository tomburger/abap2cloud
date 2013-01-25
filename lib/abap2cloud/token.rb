class Token
  
  DOT = :token_dot
  WORD = :token_word
  NUMBER = :token_number
  STRING = :token_string
  COLLON = :token_collon
  SLASH = :token_slash
  PUNCTION = :token_punction
  EQUAL = :token_equal
  COMMA = :token_comma
  
  attr_reader :kind, :value
  def initialize(kind, value='')
    @kind = kind
    @value = value
  end
  def to_s
    return @value.empty? ? @kind.to_s : "#{@kind} (#{@value})"
  end
end
