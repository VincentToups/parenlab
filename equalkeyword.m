function [r] = equalkeyword(input)
%Parse one lisp-style keyword.
[ s e ] = regexp(input, '^:[-+:<>?.{}|*/!@$%^&*_a-zA-Z0-9]+');
if ~(isempty(s))
  theKeyword.type = 'keyword';
  theKeyword.name = input(((s):(e)));
  rest = input(((plus(e, 1)):(end)));
  r = cellArray(theKeyword, rest);
else
  r = cellArray();
end
;

end