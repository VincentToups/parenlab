function [result] = equalnumberPhStyle(input)
%Parse a number with - in the place of .
[ s e ] = regexp(input, '^[+-]{0,1}[0-9]*[-]{0,1}[0-9]*');
if ~(isempty(s))
  result = cellArray(str2double(strrep(input(((s):(e))), '-', '.')), input(((plus(1, e)):(end))));
else
  setq(result(cellArray()));
end
;

end