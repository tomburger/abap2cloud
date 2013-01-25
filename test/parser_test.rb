require 'test_helper'

$LOAD_PATH << File.expand_path(File.dirname(__FILE__) + '/../lib')

require 'abap2cloud.rb'

class ParserTest < Test::Unit::TestCase

  def setup
    @tokens = []
  end

  def teardown
  end
  
  def call
    yield @tokens
    @result = Parser.run(@tokens).print(false)
  end  
  def parse(prg)
    prg_lines = []
    prg_lines << prg
    Tokenizer.run(prg_lines) { |t| @tokens << t }
    @result = Parser.run(@tokens).print(false)
  end
  def assert_token(ix, kind, value='')
    assert_equal kind, @tokens[ix].kind 
    assert_equal value, @tokens[ix].value
  end

  def test_collons
    # WRITE: A, B. ==> WRITE A. WRITE B.
    @tokens << Token.new(Token::WORD, 'WRITE') << Token.new(Token::COLLON)
    @tokens << Token.new(Token::WORD, 'A') << Token.new(Token::COMMA)
    @tokens << Token.new(Token::WORD, 'B') << Token.new(Token::DOT)
    @tokens = Parser.remove_collons(@tokens)
    assert_equal 6, @tokens.size
    assert_token 0, Token::WORD, 'WRITE'
    assert_token 1, Token::WORD, 'A'
    assert_token 2, Token::DOT
    assert_token 3, Token::WORD, 'WRITE'
    assert_token 4, Token::WORD, 'B'
    assert_token 5, Token::DOT
  end
  def test_collons_with_slash
    # WRITE: / A, B. ==> WRITE / A. WRITE B.
    @tokens << Token.new(Token::WORD, 'WRITE') << Token.new(Token::COLLON)
    @tokens << Token.new(Token::SLASH)
    @tokens << Token.new(Token::WORD, 'A') << Token.new(Token::COMMA)
    @tokens << Token.new(Token::WORD, 'B') << Token.new(Token::DOT)
    @tokens = Parser.remove_collons(@tokens)
    assert_equal 7, @tokens.size
    assert_token 0, Token::WORD, 'WRITE'
    assert_token 1, Token::SLASH
    assert_token 2, Token::WORD, 'A'
    assert_token 3, Token::DOT
    assert_token 4, Token::WORD, 'WRITE'
    assert_token 5, Token::WORD, 'B'
    assert_token 6, Token::DOT
  end
  
  def test_with_program
    call do |t|
      t << Token.new(Token::WORD, 'PROGRAM') << Token.new(Token::WORD, 'TEST') << Token.new(Token::DOT)
    end
    assert_equal 'PROGRAM(TEST)', @result
  end
  def test_with_write
    call do |t|
      t << Token.new(Token::WORD, 'WRITE') << Token.new(Token::COLLON) 
      t << Token.new(Token::STRING, 'Hay How') << Token.new(Token::COMMA) 
      t << Token.new(Token::WORD, 'A-B') << Token.new(Token::DOT)
    end
    assert_equal "PROGRAM(STANDARD)[WRITE( 'Hay How' color= ),WRITE( A-B color= )]", @result
  end
  def test_write_with_color
    call do |t|
      t << Token.new(Token::WORD, 'WRITE') 
      t << Token.new(Token::SLASH) << Token.new(Token::STRING, 'Hey there.') 
      t << Token.new(Token::WORD, 'COLOR') << Token.new(Token::WORD, 'COL_TOTAL') 
      t << Token.new(Token::DOT)
    end
    assert_equal "PROGRAM(STANDARD)[WRITE( 'Hey there.' color=COL_TOTAL newline=true )]", @result
  end    
  def test_program_with_write
    call do |t|
      t << Token.new(Token::WORD, 'PROGRAM') << Token.new(Token::WORD, 'TEST') << Token.new(Token::DOT) 
      t << Token.new(Token::WORD, 'WRITE') << Token.new(Token::STRING, 'Ha!') << Token.new(Token::DOT) 
    end
    assert_equal "PROGRAM(TEST)[WRITE( 'Ha!' color= )]", @result
  end
  def test_prog_structure
    call do |t|
      t << Token.new(Token::WORD, 'PROGRAM') << Token.new(Token::WORD, 'TEST') << Token.new(Token::DOT) 
      t << Token.new(Token::WORD, 'START-OF-SELECTION') << Token.new(Token::DOT) 
      t << Token.new(Token::WORD, 'END-OF-SELECTION') << Token.new(Token::DOT) 
    end
    assert_equal "PROGRAM(TEST)", @result # events safely ignored...
  end
  def test_expression
    call do |t|
      t << Token.new(Token::WORD, 'A') << Token.new(Token::EQUAL)
      t << Token.new(Token::WORD, '1') << Token.new(Token::PUNCTION, '+')
      t << Token.new(Token::WORD, 'B-C') << Token.new(Token::DOT)
    end
    assert_equal "PROGRAM(STANDARD)[COMPUTE(A,EXPR(1 + B-C))]", @result
  end
  def test_simple_program
    parse <<-EOF
      program test.
      a = b + c.
      write: x, y.
    EOF
    assert_equal "PROGRAM(TEST)[COMPUTE(A,EXPR(B + C)),WRITE( X color= ),WRITE( Y color= )]", @result
  end
  def test_data_declaration
    parse <<-EOF
      program test.
      data a type i.
    EOF
    assert_equal "PROGRAM(TEST)[VAR(A,I)]", @result
  end
  def test_parse_if
    parse <<-EOF
      program test.
      data a type i.
      if a > 1.
        write 'Ahoj!'.
      endif.
    EOF
    assert_equal "PROGRAM(TEST)[VAR(A,I),IF(EXPR(A > 1),[WRITE( 'Ahoj!' color= )],)]", @result
  end
  def test_parse_if_else
    parse <<-EOF
      program test.
      data a type i.
      if a gt 1.
        write 'Vetsi'.
      else.
        write 'Mensi'.
      endif.
    EOF
    assert_equal "PROGRAM(TEST)[VAR(A,I),IF(EXPR(A GT 1),[WRITE( 'Vetsi' color= )],[WRITE( 'Mensi' color= )])]", @result
  end
  
  # negative tests...
  def test_missing_dot
    assert_raise(RuntimeError) do
      call do |t|
        t << Token.new(Token::WORD, 'PROGRAM') << Token.new(Token::WORD, 'TEST')
      end
    end
  end    
  def test_unknown_command
    assert_raise(RuntimeError) do
      parse <<-EOF
        program test.
        one two three.
      EOF
    end
  end
  def test_if_without_end
    assert_raise(RuntimeError) do
      parse <<-EOF
        program test.
        if 5 gt 3.
          write 'OK'.
      EOF
    end
  end
end
