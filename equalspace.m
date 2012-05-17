function [r] = equalspace(input)
%Parse a single space.
f = first(input);
if any(strcmp(f, cellArray(' ', sprintf('\t'), sprintf('\n'))))
  r = cellArray(f, input(((2):(end))));
else
  r = cellArray();
end
;

end