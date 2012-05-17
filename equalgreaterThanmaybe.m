function [r] = equalgreaterThanmaybe(equalp)
%Return a parser that parses =p, and if that fails succeeds anyway,
%returning an empty value.
r = @lambdaminus77728;
function [retvalminus77729] = lambdaminus77728(input)
%
retvalminus77729 = funcall(@lambdaminus77730, funcall(equalp, input));
function [retvalminus77731] = lambdaminus77730(r1)
%
retvalminus77731 = fif(~(isempty(r1)), @()r1, @()cellArray([], input));

end

end

end