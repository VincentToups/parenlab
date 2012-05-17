function [equalp] = equalgreaterThanzeroOrMore(equalip)
%Return a parser which parses zero or more =ips.  A list of the
%successfully parsed results is returned.
@equalgreaterThanzeroOrMoreInner;
equalp = @equalgreaterThanzeroOrMoreInner;
function [pval] = equalgreaterThanzeroOrMoreInner(input)
%
result = cellArray();
done = 0;
while ~(done)
  oneResult = funcall(equalip, input);
  if ~(isempty(oneResult))
    input = second(oneResult);
    result{plus(end, 1)} = first(oneResult);
  else
    pval = cellArray(result, input);
    done = 1;
  end
;
end
;

end

end