function [r] = equalgreaterThanmaybe(equalp)
%Return a parser that parses =p, and if that fails succeeds anyway,
%returning an empty value.
r = @lambdaminus82867;
function [retvalminus82868] = lambdaminus82867(input)
%
retvalminus82868 = funcall(@lambdaminus82869, funcall(equalp, input));
function [retvalminus82870] = lambdaminus82869(r1)
%
retvalminus82870 = fif(~(isempty(r1)), @()r1, @()cellArray([], input));

end

end

end