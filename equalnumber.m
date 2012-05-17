function [result] = equalnumber(input)
%Parse a single number from the input stream.
[ s e ] = regexp(input, '^[+-]{0,1}[0-9]*[.]{0,1}[0-9]*');
if ~(isempty(s))
  result = cellArray(str2double(input(((s):(e)))), input(((plus(1, e)):(end))));
else
  result = cellArray();
end
;

end