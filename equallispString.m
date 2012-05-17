function [r] = equallispString(input)
%
if isempty(input)
  r = cellArray();
elseif strcmp('"', input(1))
  input = input(((2):(end)));
  qIndexes = equalequal([ input ' ' ], '"');
  eIndexes = equalequal([ ' ' input ], '\');
  termCands = find(ampersand(~(eIndexes), qIndexes));
  if isempty(termCands)
    r = cellArray();
  elseif 'otherwise'
    term = termCands(1);
    stringContents = input(((1):(minus(term, 1))));
    r = cellArray(stringContents, input(((plus(1, term)):(end))));
  end
  ;
elseif 'otherwise'
  r = cellArray();
end
;

end