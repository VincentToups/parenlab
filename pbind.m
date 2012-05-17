function [oparser] = pbind(monadicValue, monadicFunction)
%Bind operation for deterministic string oparser.
if ischar(monadicValue)
  monadicValue = equalgreaterThanstring(monadicValue);
end
;
if isnumeric(monadicValue)
  monadicValue = equalgreaterThannumber(monadicValue);
end
;
@innerParser;
oparser = @innerParser;
function [pstate] = innerParser(input)
%
result = funcall(monadicValue, input);
if ~(iscell(result))
  error(sprintf('Parsers must return a cell array. Input is: %s', input));
end
;
if ~(isempty(result))
  value = first(result);
  rest = second(result);
  newParser = funcall(monadicFunction, value);
  pstate = funcall(newParser, rest);
else
  pstate = cellArray();
end
;

end

end