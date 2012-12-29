class Command 
  def initialize(name)
    @name = name
  end
  def print(pretty)
    @name
  end
end

class CmdSequence < Command
  def initialize
    super('')
    @commands = []
  end
  def add(cmd)
    @commands << cmd
  end
  def print(pretty)
    return '' if @commands.empty? 
    seq = @commands.collect { |c| c.print(pretty) }.join(pretty ? "\n" : ',')
    return pretty ? "[\n#{seq}\n]" : "[#{seq}]"
  end
end

class CmdProgram < Command
  def initialize(prog_name)
    super('PROGRAM')
    @prog_name = prog_name
    @sequence = CmdSequence.new
  end
  def print(pretty)
    s1 = super(pretty) + '(' + @prog_name + ')'
    s2 = @sequence.print(pretty)
    s1 + s2
  end
  def add(cmd)
    @sequence.add(cmd)
  end
end

class CmdWrite < Command
  def initialize(arg)
    super('WRITE')
    @arg = arg
  end
  def print(pretty)
    s = super(pretty)
    case @arg.kind
    when Token::STRING
      s << ' \'' << @arg.value << '\''
    when Token::WORD
      s << ' ' << @arg.value
    end
    return s
  end
end

class CmdExpr < Command
  def initialize(target)
    super('EXPR')
    @target = target
    @expression = []
  end
  def add(token)
    @expression << token
  end
  def print(pretty)
    s = super(pretty)
    exp = @expression.collect { |t| t.value }.join(' ')
    return "#{s}(#{@target},#{exp})"
  end
end

class CmdVar < Command
  def initialize(var, type)
    super('VAR')
    @var = var
    @type = type
  end
  def print(pretty)
    s = super(pretty)
    return "#{s}(#{@var},#{@type})"
  end
end
