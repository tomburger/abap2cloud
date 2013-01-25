require 'test_helper'

$LOAD_PATH << File.expand_path(File.dirname(__FILE__) + '/../lib')

require 'abap2cloud.rb'

class TokenizerTest < Test::Unit::TestCase

  def setup
    @lines = []
    @tokens = []
  end

  def teardown
  end
  
  def call
    yield @lines
    Tokenizer.run(@lines) { |t| @tokens << t }
  end    
  def assert_token(ix, kind, value='')
    assert_equal kind, @tokens[ix].kind 
    assert_equal value, @tokens[ix].value
  end

  def test_keyword
    call { |l| l << 'program Test.' }
    assert_equal 3, @tokens.size
    assert_token 0, Token::WORD, 'PROGRAM'
    assert_token 1, Token::WORD, 'TEST'
    assert_token 2, Token::DOT
  end
  
  def test_write
    call { |l| l << "write: / 'How are you?', X." }
    assert_equal 7, @tokens.size
    assert_token 0, Token::WORD, 'WRITE'
    assert_token 1, Token::COLLON
    assert_token 2, Token::SLASH 
    assert_token 3, Token::STRING,'How are you?'
    assert_token 4, Token::COMMA
    assert_token 5, Token::WORD, 'X'
    assert_token 6, Token::DOT
  end
  
  def test_expression
    call { |l| l << "a = 1 + c-b - x / 2." }
    assert_equal 10, @tokens.size
    assert_token 0, Token::WORD, 'A'
    assert_token 1, Token::EQUAL
    assert_token 2, Token::NUMBER, '1'
    assert_token 3, Token::PUNCTION, '+'
    assert_token 4, Token::WORD, 'C-B'
    assert_token 5, Token::PUNCTION, '-'
    assert_token 6, Token::WORD, 'X'
    assert_token 7, Token::PUNCTION, '/'
    assert_token 8, Token::NUMBER, '2'
    assert_token 9, Token::DOT
  end
  
  def test_variable
    call { |l| l << "data a type i." }
    assert_equal 5, @tokens.size
    assert_token 0, Token::WORD, 'DATA'
    assert_token 1, Token::WORD, 'A'
    assert_token 2, Token::WORD, 'TYPE'
    assert_token 3, Token::WORD, 'I'
    assert_token 4, Token::DOT
  end
  
  def test_open_string
    assert_raise(RuntimeError) do
      call { |l| l << "a = 'This string has no end" }
    end
  end
  
  def test_multiline_expression
    call do |l|
      l << "a = b"
      l << "  + c"
      l << "."
    end
    assert_equal 6, @tokens.size
    assert_token 0, Token::WORD, 'A'
    assert_token 1, Token::EQUAL
    assert_token 2, Token::WORD, 'B'
    assert_token 3, Token::PUNCTION, '+'
    assert_token 4, Token::WORD, 'C'
    assert_token 5, Token::DOT
  end
  def test_multiline_with_slash
    call do |l|
      l << "a = b /"
      l << "  c."
    end
    assert_equal 6, @tokens.size
    assert_token 0, Token::WORD, 'A'
    assert_token 1, Token::EQUAL
    assert_token 2, Token::WORD, 'B'
    assert_token 3, Token::PUNCTION, '/'
    assert_token 4, Token::WORD, 'C'
    assert_token 5, Token::DOT
  end
  
end
