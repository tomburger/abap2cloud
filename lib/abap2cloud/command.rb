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
  def compile
    @commands.collect { |c| c.compile }.join("\n")
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
    s = @sequence.compile 
    return <<-EOF
<?php
$writer = array();
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
<?php
$ix = 0;
foreach($writer as $line) {
  if ($ix > 0 && $line['newline']) echo '<br/>';
  echo $line['value'] . ' ';
  $ix++;
}
?>
</div>
</body>
</html>
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
  def compile
    s = 'array_push($writer, array('
    s << "'newline' => #{@newline},"
    s << "'color' => #{@color}," if @color
    case @arg.kind
    when Token::WORD
      s << "'value' => $#{@arg.value}));"
    when Token::STRING
      s << "'value' => '#{@arg.value}'));"
    end
    return s
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
  def compile
    @tokens.collect { |t| token_to_expr(t) }.join(' ')
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
  def compile
    e = @expression.compile
    "$#{@target} = #{e};"
  end
end

class CmdIf < Command
  def initialize(expr)
    super('IF')
    @expression = expr
    @true_part = CmdSequence.new
    @false_part = nil
  end
  def add(cmd)
    if @false_part.nil?
      @true_part.add(cmd)
    else
      @false_part.add(cmd)
    end
  end
  def startElse
    @false_part = CmdSequence.new
  end
  def print(pretty)
    s = super(pretty)
    e = @expression.print(pretty)
    t = @true_part.print(pretty)
    f = @false_part.nil? ? '' : @false_part.print(pretty)
    "#{s}(#{e},#{t},#{f})"
  end
  def compile
    s = []
    s << 'if('
    s << @expression.compile
    s << '){'
    s << @true_part.compile
    s << '}'
    if !@false_part.nil?
      s << 'else {'
      s << @false_part.compile
      s << '}'
    end
    return s.join("\n")
  end
end

class CmdType < Command
  STD = :table_std
  
  def initialize(table, name)
    super('TYPE')
    @table = table
    @name = name
  end
  def print(pretty)
    if @table.nil? 
      @name
    else
      case @table
      when STD
        tab = 'STD'
      end
      "TABLE(#{tab},#{@name})"
    end
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
    return "#{s}(#{@var},#{@type.print(pretty)})"
  end
  def compile
    "$#{@var} = 0;"
  end
end
