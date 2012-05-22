function [r] = equalgreaterThanmaybe(equalp)
%Return a parser that parses =p, and if that fails succeeds anyway,
%returning an empty value.
r = @lambdaminus108384;
function [retvalminus108385] = lambdaminus108384(input)
%
retvalminus108385 = funcall(@lambdaminus108386, funcall(equalp, input));
function [retvalminus108387] = lambdaminus108386(r1)
%
retvalminus108387 = fif(~(isempty(r1)), @()r1, @()cellArray([], input));

end

end

end