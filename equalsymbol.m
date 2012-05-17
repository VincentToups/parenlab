function [r] = equalsymbol(input)
%Parse one lisp-style symbol.
[ s e ] = regexp(input, '^[-+:<>?.{}|*/!@$%^&*_a-zA-Z][-+:<>?.{}|*/!@$%^&*_a-zA-Z0-9]+');
if ~(isempty(s))
  theSymbol.type = 'symbol';
  theSymbol.name = input(((s):(e)));
  rest = input(((plus(e, 1)):(end)));
  r = cellArray(theSymbol, rest);
else
  r = cellArray();
end
;

end