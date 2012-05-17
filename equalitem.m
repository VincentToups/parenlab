function [result] = equalitem(input)
%A parser which takes the first value off the input and returns it.
%If the input is empty, this fails.
if ~(isempty(input))
  result = cellArray(input(1), input(((2):(end))));
else
  result = cellArray();
end
;

end