class Parser
  def self.run(tokens)
    c = nil
    tokens = remove_collons(tokens)
    token = tokens[0]
    if check(token, Token::WORD, 'PROGRAM')
      remove(tokens, Token::WORD, 'PROGRAM')
      c = CmdProgram.new(tokens.shift.value)
      remove(tokens, Token::DOT)
    else
      c = CmdProgram.new('STANDARD')
    end
    sequence(c, tokens)
    return c
  end
  def self.find_stopword(tokens, stopwords)
    if stopwords.nil?
      tokens.empty? ? 'END-OF-PROGRAM' : nil
    else
      raise "Missing one of stopwords #{stopwords.join(',')}" if tokens.empty?
      token = tokens[0]
      if !stopwords.find_index { |s| check(token, Token::WORD, s) }
        nil
      else
        remove(tokens, Token::WORD)
        token.value
      end
    end
  end
  def self.sequence(parent, tokens, stopwords = nil)
    last_length = tokens.size + 1
    while !(stopword = find_stopword(tokens, stopwords)) 

      # invariant - with each loop, there is less tokens
      raise "Seems like endless loop!" if tokens.size == last_length
      last_length = tokens.size
      
      token = tokens[0]
      case 
      when check(token, Token::WORD, 'WRITE')
        remove(tokens, Token::WORD, 'WRITE')
        c = write(tokens)
        parent.add(c)
      when check(token, Token::WORD, 'START-OF-SELECTION')
        # skip this all together...
        remove(tokens, Token::WORD, 'START-OF-SELECTION')
        remove(tokens, Token::DOT)
      when check(token, Token::WORD, 'END-OF-SELECTION')
        # skip this all together...
        remove(tokens, Token::WORD, 'END-OF-SELECTION')
        remove(tokens, Token::DOT)
      when check(token, Token::WORD, 'DATA')
        remove(tokens, Token::WORD, 'DATA')
        c = variable(tokens)
        parent.add(c)
      when check(token, Token::WORD, 'IF')
        remove(tokens, Token::WORD, 'IF')
        c = if_command(tokens)
        parent.add(c)
      else
        if check(tokens[1], Token::EQUAL)
          remove(tokens, Token::WORD, token.value)
          remove(tokens, Token::EQUAL)
          c = compute(token.value, tokens)
          parent.add(c)
        else
          raise "Unknown command #{token}"
        end
      end
    end
    return stopword
  end
  def self.check(token, kind, value='')
    token.kind == kind && token.value == value
  end
  def self.remove(tokens, kind, value=nil)
    token = tokens.shift
    raise "Missing token #{kind}" if token.nil? 
    raise "Expecting #{kind}, but #{token} found" if token.kind != kind
    raise "Expecting #{value}, but #{token.value} found" if !value.nil? && value != token.value
  end
  def self.until_dot(tokens)
    a = []
    while token = tokens.shift
      break if token.kind == Token::DOT
      a << token
    end
    a
  end
  def self.expression(tokens)
    CmdExpression.new(until_dot(tokens))
  end
  def self.compute(target, tokens)
    CmdCompute.new(target, expression(tokens))
  end
  def self.type(tokens)
    table = nil
    token = tokens.shift
    if (check(token, Token::WORD, 'STANDARD'))
      table = CmdType::STD
      remove(tokens, Token::WORD, 'TABLE')
      remove(tokens, Token::WORD, 'OF')
      token = tokens.shift
    end
    remove(tokens, Token::DOT)
    CmdType.new(table, token.value)
  end
  def self.variable(tokens)
    v = tokens.shift             # variable
    remove(tokens, Token::WORD, 'TYPE')  # keyword TYPE
    t = type(tokens)             
    return CmdVar.new(v.value, t)
  end
  def self.write(tokens)
    token = tokens.shift
    if check(token, Token::SLASH)
      c = CmdWrite.new(true, tokens.shift)
    else
      c = CmdWrite.new(false, token)
    end
    token = tokens[0]
    if check(token, Token::WORD, 'COLOR')
      remove(tokens, Token::WORD, 'COLOR')
      c.color = tokens.shift.value
    end
    remove(tokens, Token::DOT)
    return c
  end
  def self.if_command(tokens)
    e = expression(tokens)
    c = CmdIf.new(e)
    s = sequence(c, tokens, ['ELSE', 'ENDIF'])
    remove(tokens, Token::DOT)
    if s === 'ELSE'
      c.startElse
      sequence(c, tokens, ['ENDIF'])
      remove(tokens, Token::DOT)
    end
    return c
  end
  def self.remove_collons(old)
    new = []
    cmd = []
    intro = []
    old.each do |c|
      case c.kind
      when Token::DOT
        new.concat(intro).concat(cmd) << c
        intro = []
        cmd = []
      when Token::COLLON
        intro = Array.new(cmd)
        cmd = []
      when Token::COMMA
        new.concat(intro).concat(cmd) << Token.new(Token::DOT)
        cmd = []
      else
        cmd << c
      end
    end
    new.concat(intro) if !intro.empty?
    new.concat(cmd) if !cmd.empty?
    return new
  end
end
