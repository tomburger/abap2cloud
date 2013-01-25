class Parser
  def self.run(tokens)
    c = nil
    tokens = remove_collons(tokens)
    token = tokens[0]
    if check(token, Token::WORD, 'PROGRAM')
      remove(tokens, Token::WORD)
      c = CmdProgram.new(tokens.shift.value)
      remove(tokens, Token::DOT)
    else
      c = CmdProgram.new('STANDARD')
    end
    sequence(c, tokens)
    return c
  end
  def self.end_of_seq(tokens, stopword)
    if stopword === ''
      tokens.empty?
    else
      raise "Missing stopword #{stopword}" if tokens.empty?
      token = tokens[0]
      if check(token, Token::WORD, stopword)
        remove(tokens, Token::WORD)
        true
      else
        false
      end
    end
  end
  def self.sequence(parent, tokens, stopword = '')
    last_length = tokens.size + 1
    while !end_of_seq(tokens, stopword) 

      # invariant - with each loop, there is less tokens
      raise "Seems like endless loop!" if tokens.size == last_length
      last_length = tokens.size
      
      token = tokens[0]
      case 
      when check(token, Token::WORD, 'WRITE')
        remove(tokens, Token::WORD)
        c = write(tokens)
        parent.add(c)
      when check(token, Token::WORD, 'START-OF-SELECTION')
        # skip this all together...
        remove(tokens, Token::WORD)
        remove(tokens, Token::DOT)
      when check(token, Token::WORD, 'END-OF-SELECTION')
        # skip this all together...
        remove(tokens, Token::WORD)
        remove(tokens, Token::DOT)
      when check(token, Token::WORD, 'DATA')
        remove(tokens, Token::WORD)
        c = variable(tokens)
        parent.add(c)
      when check(token, Token::WORD, 'IF')
        remove(tokens, Token::WORD)
        e = expression(tokens)
        c = CmdIf.new(e)
        sequence(c, tokens, 'ENDIF')
        remove(tokens, Token::DOT)
        parent.add(c)
      else
        if check(tokens[1], Token::EQUAL)
          remove(tokens, Token::WORD)
          remove(tokens, Token::EQUAL)
          c = compute(token.value, tokens)
          parent.add(c)
        else
          raise "Unknown command #{token}"
        end
      end
    end
  end
  def self.check(token, kind, value='')
    token.kind == kind && token.value == value
  end
  def self.remove(tokens, kind)
    token = tokens.shift
    raise "Missing token #{kind}" if token == nil 
    raise "Expecting #{kind}, but #{token} found" if token.kind != kind
  end
  def self.expression(tokens)
    a = []
    while token = tokens.shift
      break if token.kind == Token::DOT
      a << token
    end
    CmdExpression.new(a)
  end
  def self.compute(target, tokens)
    CmdCompute.new(target, expression(tokens))
  end
  def self.variable(tokens)
    v = tokens.shift             # variable
    remove(tokens, Token::WORD)  # keyword TYPE
    t = tokens.shift             # type
    remove(tokens, Token::DOT)
    return CmdVar.new(v.value, t.value)
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
      remove(tokens, Token::WORD)
      c.color = tokens.shift.value
    end
    remove(tokens, Token::DOT)
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
