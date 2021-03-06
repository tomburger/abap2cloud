class Tokenizer
  def self.add(word)
    if word =~ /^\d+(\.\d+)?$/
      Token.new(Token::NUMBER, word)
    else
      Token.new(Token::WORD, word.upcase)
    end
  end
  def self.run(lines)
    lines.each do |l|
      word = ''
      string = false
      expression = false
      l.chars do |c|
        if string
          if c.eql?('\'')
            yield Token.new(Token::STRING, word)
            word = ''
            string = false
          else
            word << c
          end
        else
          case c
          when /[\w\-\+\>\<]/
            word << c
          when /\s/
            if !word.empty?
              if word =~ /^\w([\w\-]*)$/
                yield add word 
              else
                yield Token.new(Token::PUNCTION, word.upcase)
              end
              word = ''
            end
          when '\''
            string = true
          when '='
            yield Token.new(Token::EQUAL)
            expression = true
          when '/'
            if !word.empty?
              yield add word 
              word = ''
            end
            if expression
              yield Token.new(Token::PUNCTION, '/')
            else
              yield Token.new(Token::SLASH)
            end
          when ':'
            if !word.empty?
              yield add word 
              word = ''
            end
            yield Token.new(Token::COLLON)
          when ','
            if !word.empty?
              yield add word 
              word = ''
            end
            yield Token.new(Token::COMMA)
          when '.'
            if !word.empty?
              yield add word 
              word = ''
            end
            yield Token.new(Token::DOT)
            expression = false
          end
        end
      end
      raise "String is not terminated" if string
      if !word.empty?
        yield add word 
        word = ''
      end
    end
  end
end
