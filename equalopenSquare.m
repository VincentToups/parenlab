function [r] = equalopenSquare(input)
%
if strcmp(input(1), '[')
  r = cellArray('[', input(((2):(end))));
else
  r = cellArray();
end
;

end