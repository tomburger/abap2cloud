class Command 
  def initialize(name)
    @name = name
  end
  def print(pretty)
    @name
  end
  def compile
    return 'PHP, here we go!!'
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
  def compile(spool)
    @commands.collect { |c| c.compile(spool) }.join("\n")
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
  def compile
    spool = []
    s = @sequence.compile spool
    return <<-EOF
<?php
      #{s}
      ?><html><head>
      <title>ABAP-to-Cloud</title>
      <style>
      .write { font-family: Courier; border: 1px solid black; background-color: silver; }
      </style>
      </head>
      <body>
      <h1>ABAP-to-Cloud</h1>
      <hr/>
      <div class="write">
      #{spool.join("\n")}
      </div>
      </body>
      </html>
      <?

?>
EOF
  end
  def add(cmd)
    @sequence.add(cmd)
  end
end

class CmdWrite < Command
  attr_writer :color
  def initialize(newline, arg)
    super('WRITE')
    @arg = arg
    @newline = newline
  end
  def print(pretty)
    s = super(pretty)
    s << '('
    case @arg.kind
    when Token::STRING
      s << ' \'' << @arg.value << '\''
    when Token::WORD
      s << ' ' << @arg.value
    end
    s << " color=#{@color}"
    s << " newline=true" if @newline
    s << ' )'
    return s
  end
  def compile(spool)
    s = ''
    s << "<br/>" if @newline

    t = ''
    case @arg.kind
    when Token::WORD
      t = "<?php echo $#{@arg.value} ?>"
    when Token::STRING
      t = @arg.value
    end
    t = "<font color='#{@color}'>#{t}</font>" if (@color)

    s << t
    spool << s
    
    return ''
  end
end

class CmdExpression < Command
  def initialize(tokens)
    super('EXPR')
    @tokens = tokens
  end
  def print(pretty)
    s = super(pretty)
    e = @tokens.collect { |t| t.value }.join(' ')
    "#{s}(#{e})"
  end
  def token_to_expr(token)
    if token.kind == Token::WORD   # means variable!
      '$' + token.value
    else
      token.value
    end
  end
  def compile(spool)
    @expression.collect { |t| token_to_expr(t) }.join(' ')
  end
end

class CmdCompute < Command
  def initialize(target, expression)
    super('COMPUTE')
    @target = target
    @expression = expression
  end
  def print(pretty)
    s = super(pretty)
    e = @expression.print(pretty)
    return "#{s}(#{@target},#{e})"
  end
  def compile(spool)
    e = @expression.compile(spool)
    "$#{@target} = #{e};"
  end
end

class CmdIf < Command
  def initialize(expr)
    super('IF')
    @expression = expr
    @true_part = CmdSequence.new
  end
  def add(cmd)
    @true_part.add(cmd)
  end
  def print(pretty)
    s = super(pretty)
    e = @expression.print(pretty)
    t = @true_part.print(pretty)
    "#{s}(#{e},#{t},)"
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
  def compile(spool)
    "$#{@var} = 0;"
  end
end
