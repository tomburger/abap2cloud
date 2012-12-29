class Parser
  def self.run(tokens)
    c = nil
    tokens = remove_collons(tokens)
    token = tokens[0]
    if check(token, Token::WORD, 'PROGRAM')
      tokens.shift  # remove keyword
      c = CmdProgram.new(tokens.shift.value)
      tokens.shift  # removes dot
    else
      c = CmdProgram.new('STANDARD')
    end
    sequence(c, tokens)
    return c
  end
  def self.sequence(parent, tokens)
    last_length = tokens.size + 1
    while !tokens.empty?

      # invariant - with each loop, there is less tokens
      raise "Seems like endless loop!" if tokens.size == last_length
      last_length = tokens.size
      
      token = tokens[0]
      case 
      when check(token, Token::WORD, 'WRITE')
        tokens.shift # remove keyword
        c = CmdWrite.new(tokens.shift)
        tokens.shift  # removes dot
        parent.add(c)
      when check(token, Token::WORD, 'START-OF-SELECTION')
        # skip this all together...
        tokens.shift; tokens.shift
      when check(token, Token::WORD, 'END-OF-SELECTION')
        tokens.shift; tokens.shift
      when check(token, Token::WORD, 'DATA')
        tokens.shift
        c = variable(tokens)
        parent.add(c)
      else
        if check(tokens[1], Token::EQUAL)
          tokens.shift; tokens.shift
          c = expression(token.value, tokens)
          parent.add(c)
        else
          # error!!!
        end
      end
    end
  end
  def self.check(token, kind, value='')
    token.kind == kind && token.value == value
  end
  def self.expression(target, tokens)
    c = CmdExpr.new(target)
    while token = tokens.shift
      break if token.kind == Token::DOT
      c.add token
    end
    return c
  end
  def self.variable(tokens)
    v = tokens.shift  # variable
    tokens.shift      # removing keyword TYPE
    t = tokens.shift  # type
    tokens.shift      # remove dot
    return CmdVar.new(v.value, t.value)
  end
  def self.remove_collons(old)
    new = []
    cmd = []
    intro = []
    old.each do |c|
      case c.kind
      when Token::DOT
        new.concat(intro).concat(cmd) << c
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
    return new
  end
end
