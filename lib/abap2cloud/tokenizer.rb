class Tokenizer
  def self.run(lines)
    lines.each do |l|
      word = ''
      string = false
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
          when /[\w\-\+]/
            word << c
          when /\s/
            if !word.empty?
              if word =~ /^(\w+)$/
                yield Token.new(Token::WORD, word.upcase)
              else
                yield Token.new(Token::PUNCTION, word.upcase)
              end
              word = ''
            end
          when '\''
            string = true
          when '='
            yield Token.new(Token::EQUAL)
          when '/'
            if !word.empty?
              yield Token.new(Token::WORD, word.upcase)
              word = ''
            end
            yield Token.new(Token::SLASH)
          when ':'
            if !word.empty?
              yield Token.new(Token::WORD, word.upcase)
              word = ''
            end
            yield Token.new(Token::COLLON)
          when '.'
            if !word.empty?
              yield Token.new(Token::WORD, word.upcase)
              word = ''
            end
            yield Token.new(Token::DOT)
          end
        end
      end
    end
  end
end
