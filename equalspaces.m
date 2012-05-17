function [r] = equalspaces(input)
%
[ s e ] = regexp(input, '^ +');
if ~(isempty(s))
  r = cellArray(input(((s):(e))), input(((plus(e, 1)):(end))));
else
  r = cellArray();
end
;

end