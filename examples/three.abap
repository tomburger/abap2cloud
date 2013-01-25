program Test_Three.

data a type i.
data b type i.

start-of-selection.

  a = 3.
  b = 5.

end-of-selection.

  write: / 'Results, here we go'.
  
  if a > b.
    write: 'A greater'.
  else.
    write: 'B greater'.
  endif.
  
  write: / 'Nothing more to print'.
