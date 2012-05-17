function [equalp] = equalgreaterThanor(varargin)
%Return a parser which tries each of the parser arguments in turn
%until one succeeds.  If none succeed, then this parser also fails.
@equalgreaterThanorInner;
equalp = @equalgreaterThanorInner;
function [pval] = equalgreaterThanorInner(input)
%
for item = flatAcross(varargin)
  item = item{1};
  if ~(strcmp('function_handle', class(item)))
    error('Really expected all parsers to be functions.');
  end
  ;
  result = funcall(item, input);
  if ~(isempty(result))
    pval = result;
    return;
  end
  ;
end
;
pval = cellArray();

end

end