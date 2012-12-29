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
    call { |l| l << "write: / 'How are you?'." }
    assert_equal 5, @tokens.size
    assert_token 0, Token::WORD, 'WRITE'
    assert_token 1, Token::COLLON
    assert_token 2, Token::SLASH 
    assert_token 3, Token::STRING,'How are you?'
    assert_token 4, Token::DOT
  end
end
